      subroutine nek_pointscloud()
      include 'SIZE'
      include 'TOTAL'
      include 'NEKMOOSE'
      integer e,f
      integer tots_np(lp), iwk(lp)

      nxyz=lx1*ly1*lz1

      nw_bd1 = 0
      do e=1,nelt
      eg = lglel(e)
      do f=1,6
        if (cbc(f,e,2).eq.'f  ') then
           nw_bd1=nw_bd1+1
           plmask(e) =1
           pflmask(e)=f
           pplist(e)=nw_bd1
        endif
      enddo
      enddo

      tots_np(nid+1)=nw_bd1
      call igop(tots_np,iwk,'+  ', np)

      offset=0
      if (nid.gt.0) then
        do i=1,nid
        offset=offset+tots_np(i)
        enddo
      endif

      do i=1,nelt
         pplist(e)=pplist(e)+offset
      enddo

      nw_bdt=0
      do i=1,np
        nw_bdt = nw_bdt + tots_np(i)
      enddo

      call assign_p(pc_x,xm1)
      call assign_p(pc_y,ym1)
      call assign_p(pc_z,zm1)

      do i=1,nw_bdt
      do j=1,4
         pc_flag((i-1)*4+j)=dble(i)
      enddo
      enddo

c
c      Uncomment for testing
c
c      if (nid.eq.0) then
c      do i=1,3
c      do j=1,4
c      k=(i-1)*4+j
c      write(6,*)i,j," - (x,y,z):",pc_x(k),pc_y(k),pc_z(k)
c      enddo
c      enddo
c      endif
c      call exitt();

      return
      end
c-----------------------------------------------------------------------
      subroutine assign_p (vpf,fu)
      include 'SIZE'
      include 'TOTAL'
      include 'NEKMOOSE'
      integer e,eg
      real*8 vpf(lsurf_m*4)
      real*8 fu(lx1,lx1,lx1,lelt)

      integer i_us, ip_stp, iv_istp

      real*8 wtmp(lsurf_m*4)

      do i=1,4*lsurf_m
      vpf(i)=0.0
      enddo

      do e=1,nelt

      if (plmask(e).eq.1)  then

      i_us=(pplist(e)-1)*4

      if (pflmask(e).eq.1) then
      vpf(i_us+1)=fu(1,1,1,e)
      vpf(i_us+2)=fu(lx1,1,1,e)
      vpf(i_us+4)=fu(1,1,lx1,e)
      vpf(i_us+3)=fu(lx1,1,lx1,e)
      endif

      if (pflmask(e).eq.2) then
      vpf(i_us+1)=fu(lx1,1,1,e)
      vpf(i_us+2)=fu(lx1,lx1,1,e)
      vpf(i_us+4)=fu(lx1,1,lx1,e)
      vpf(i_us+3)=fu(lx1,lx1,lx1,e)
      endif

      if (pflmask(e).eq.3)  then
      vpf(i_us+1)=fu(1,lx1,1,e)
      vpf(i_us+2)=fu(lx1,lx1,1,e)
      vpf(i_us+4)=fu(1,lx1,lx1,e)
      vpf(i_us+3)=fu(lx1,lx1,lx1,e)
      endif

      if (pflmask(e).eq.4) then
      vpf(i_us+1)=fu(1,1,1,e)
      vpf(i_us+2)=fu(1,lx1,1,e)
      vpf(i_us+4)=fu(1,1,lx1,e)
      vpf(i_us+3)=fu(1,lx1,lx1,e)
      endif

      if (pflmask(e).eq.5) then
      vpf(i_us+1)=fu(1,1,1,e)
      vpf(i_us+2)=fu(lx1,1,1,e)
      vpf(i_us+4)=fu(1,lx1,1,e)
      vpf(i_us+3)=fu(lx1,lx1,1,e)
      endif

      if (pflmask(e).eq.6) then
      vpf(i_us+1)=fu(1,1,lx1,e)
      vpf(i_us+2)=fu(lx1,1,lx1,e)
      vpf(i_us+4)=fu(1,lx1,lx1,e)
      vpf(i_us+3)=fu(lx1,lx1,lx1,e)
      endif

      endif
      enddo

      call  gop(vpf,wtmp,'+  ',4*nw_bdt)

      return
      end
c-----------------------------------------------------------------------
      subroutine rassign (fu,vpf,nn)
      include 'SIZE'
      include 'TOTAL'
      include 'NEKMOOSE'
      integer e,eg

      real*8 vpf(lsurf_m*4)
      real*8 fu(3,3,3,lelt)

c     Placeholder

      return
      end
C=======================================================================
      subroutine nek_interpolation()
      include 'SIZE'
      include 'TOTAL'
      include 'NEKMOOSE'
      integer e,f

      call assign_p(pc_t,t)

      return
      end
C=======================================================================
      subroutine flux_reconstruction()
      include 'SIZE'
      include 'TOTAL'
      include 'NEKMOOSE'
      integer e,f
      ntot=nx1*ny1*nz1*nelt

!     Call to rassign and spectral interpolation
      do e=1,nelt
        do f=1,6
          call surface_int(sint,sarea,flux_recon,e,f)
          if (cbc(f,e,1).eq.'W  ') then
           sint1=sint1+sint
           sarea1=sarea1+sarea
          endif
         enddo
      enddo

      call  gop(sint1,wtmp,'+  ',1)
      call  gop(sarea1,wtmp,'+  ',1)

      flux_moose=sint1/sarea1

      return
      end
c-----------------------------------------------------------------------
      subroutine nek_init_step

      include 'SIZE'
      include 'TSTEP'
      include 'INPUT'
      include 'CTIMER'

      save    icalld1
      data    icalld1 /0/
      common /cht_coupler/ pstep
      integer pstep

      if (icalld1.eq.0) then
      call nekgsync()

      if (instep.eq.0) then
        if(nid.eq.0) write(6,'(/,A,/,A,/)')
     &     ' nsteps=0 -> skip time loop',
     &     ' running solver in post processing mode'
      else
        if(nio.eq.0) write(6,'(/,A,/)') 'Starting time loop ...'
      endif

      isyc  = 0
      if(ifsync) isyc=1
      itime = 0
#ifdef TIMER
      itime = 1
#endif
      call nek_comm_settings(isyc,itime)

      ! start measurements
      call nek_comm_startstat()
      dtmp = dnekgflops()

      istep  = 0
      msteps = 1
      icalld1 = 1
      endif

      istep = istep + 1
      call nek_advance_moose
      pstep = 2

      RETURN
      END

C=======================================================================
      subroutine nek_step()

      include 'SIZE'
      include 'TSTEP'
      include 'INPUT'
      include 'CTIMER'
      common /cht_coupler/ pstep
      integer pstep

      pstep=pstep+1
      call heat(pstep)

      return
      end
C=======================================================================
      subroutine nek_finalize_step()
      include 'SIZE'
      include 'TSTEP'
      include 'INPUT'
      include 'CTIMER'

      if (filterType.eq.1) call q_filter(param(103))
      if(istep.ge.nsteps) lastep = 1
      call check_ioinfo
      call set_outfld
      call userchk
      call in_situ_check()

      if (lastep .eq. 1) then
      call nek_comm_settings(isyc,0)

      call comment

c     check for post-processing mode
      if (instep.eq.0) then
         nsteps=0
         istep=0
         if(nio.eq.0) write(6,*) 'call userchk'
         call userchk
         if(nio.eq.0) write(6,*) 'done :: userchk'
         call prepost (.true.,'his')
      else
         if (nio.eq.0) write(6,'(/,A,/)')
     $      'end of time-step loop'
      endif
      endif

      return
      end
C=======================================================================
      subroutine nek_advance_moose

      include 'SIZE'
      include 'TOTAL'
      include 'CTIMER'

      common /cgeom/ igeom

      ntot = lx1*ly1*lz1*nelv

      call nekgsync

      call setup_convect(2) ! Save conv vel

      if (iftran) call settime
      if (ifmhd ) call cfl_check
      call setsolv
      call comment

      if (ifsplit) then   ! PN/PN formulation

         do igeom=1,2

         if (igeom.gt.2) call userchk_set_xfer

         ! call here before we overwrite wx
         if (ifheat .and. ifcvode) call heat_cvode (igeom)

         if (ifgeom) then
            call gengeom (igeom)
            call geneig  (igeom)
         endif

         if (ifheat) call heat (igeom)

         if (igeom.eq.2) then
            call setprop
            call rzero(qtl,ntot)
            if (iflomach) call qthermal
         endif

         if (ifflow)          call fluid    (igeom)
         if (ifmvbd)          call meshv    (igeom)

         enddo

      else                ! PN-2/PN-2 formulation
         call setprop
         do igeom=1,2

            if (igeom.gt.2) call userchk_set_xfer

            ! call here before we overwrite wx
            if (ifheat .and. ifcvode) call heat_cvode (igeom)

            if (ifgeom) then
               if (.not.ifrich) call gengeom (igeom)
               call geneig  (igeom)
            endif

            if (ifneknekm.and.igeom.eq.2) call multimesh_create

            if (ifmhd) then
               if (ifheat)      call heat     (igeom)
                                call induct   (igeom)
            elseif (ifpert) then
               if (ifbase.and.ifheat)  call heat          (igeom)
               if (ifbase.and.ifflow)  call fluid         (igeom)
               if (ifflow)             call fluidp        (igeom)
               if (ifheat)             call heatp         (igeom)
            else 
               if (ifheat)             call heat          (igeom)
               if (ifflow)             call fluid         (igeom)
               if (ifmvbd)             call meshv         (igeom)
            endif

         enddo
      endif

      return
      end
