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

      do e=1,nelt
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
c-----------------------------------------------------------------------
      subroutine rassign (fu,vpf)
      include 'SIZE'
      include 'TOTAL'
      include 'NEKMOOSE'
      integer e,eg

      real*8 vpf(lsurf_m*4)
      real*8 fu(3,3,3,lelt)

c     Filling a lx1=3 brick for each face from MOOSE. Interpolating 
c     mid-size points at the moment. 
      
      do e=1,nelt

      do i=1,27
         fu(i,1,1,e)=0.0
      enddo 

      if (plmask(e).eq.1)  then

      i_us=(pplist(e)-1)*4

      if (pflmask(e).eq.1) then
      fu(1,1,1,e) = vpf(i_us+1)
      fu(3,1,1,e) = vpf(i_us+2)
      fu(1,1,3,e) = vpf(i_us+4)
      fu(3,1,3,e) = vpf(i_us+3)   
      fu(2,1,1,e) = 0.5*(fu(1,1,1,e)+fu(3,1,1,e)) ! interpolate midpoints 
      fu(3,1,2,e) = 0.5*(fu(3,1,1,e)+fu(3,1,3,e))  
      fu(2,1,3,e) = 0.5*(fu(1,1,3,e)+fu(3,1,3,e))
      fu(1,1,2,e) = 0.5*(fu(1,1,1,e)+fu(1,1,3,e))
      fu(2,1,2,e) = 
     &  0.25*(fu(2,1,1,e)+fu(3,1,2,e)+fu(2,1,3,e)+fu(1,1,2,e)) 
      do i=1,3         ! fill entire brick with surface solution to enable spectral interpolation
        do j=1,3
           fu(i,2,j,e)=fu(i,1,j,e)
           fu(i,3,j,e)=fu(i,1,j,e) 
        enddo
      enddo 
      endif

      if (pflmask(e).eq.2) then
      fu(3,1,1,e) = vpf(i_us+1)
      fu(3,3,1,e) = vpf(i_us+2)
      fu(3,1,3,e) = vpf(i_us+4)
      fu(3,3,3,e) = vpf(i_us+3)
      fu(3,2,1,e) = 0.5*(fu(3,1,1,e)+fu(3,3,1,e)) ! interpolate midpoints 
      fu(3,3,2,e) = 0.5*(fu(3,3,1,e)+fu(3,3,3,e))
      fu(3,2,3,e) = 0.5*(fu(3,1,3,e)+fu(3,3,3,e))
      fu(3,1,2,e) = 0.5*(fu(3,1,1,e)+fu(3,1,3,e))
      fu(3,2,2,e) =
     &  0.25*(fu(3,2,1,e)+fu(3,3,2,e)+fu(3,2,3,e)+fu(3,1,2,e))
      do i=1,3
        do j=1,3        ! fill entire brick with surface solution to enable spectral interpolation
  
           fu(2,i,j,e)=fu(3,i,j,e)
           fu(1,i,j,e)=fu(3,i,j,e)
        enddo
      enddo
      endif

      if (pflmask(e).eq.3)  then
      fu(1,3,1,e) = vpf(i_us+1)
      fu(3,3,1,e) = vpf(i_us+2)
      fu(1,3,3,e) = vpf(i_us+4)
      fu(3,3,3,e) = vpf(i_us+3)
      fu(2,3,1,e) = 0.5*(fu(1,3,1,e)+fu(3,3,1,e)) ! interpolate midpoints 
      fu(3,3,2,e) = 0.5*(fu(3,3,1,e)+fu(3,3,3,e))
      fu(2,3,3,e) = 0.5*(fu(1,3,3,e)+fu(3,3,3,e))
      fu(1,3,2,e) = 0.5*(fu(1,3,1,e)+fu(1,3,3,e))
      fu(2,3,2,e) =
     &  0.25*(fu(2,3,1,e)+fu(3,3,2,e)+fu(2,3,3,e)+fu(1,3,2,e))
      do i=1,3         ! fill entire brick with surface solution to enable spectral interpolation
        do j=1,3
           fu(i,1,j,e)=fu(i,3,j,e)
           fu(i,2,j,e)=fu(i,3,j,e)
        enddo
      enddo 
      endif

      if (pflmask(e).eq.4) then
      fu(1,1,1,e) = vpf(i_us+1)
      fu(1,3,1,e) = vpf(i_us+2)
      fu(1,1,3,e) = vpf(i_us+4)
      fu(1,3,3,e) = vpf(i_us+3)
      fu(1,2,1,e) = 0.5*(fu(1,1,1,e)+fu(1,3,1,e)) ! interpolate midpoints 
      fu(1,3,2,e) = 0.5*(fu(1,3,1,e)+fu(1,3,3,e))
      fu(1,2,3,e) = 0.5*(fu(1,1,3,e)+fu(1,3,3,e))
      fu(1,1,2,e) = 0.5*(fu(1,1,1,e)+fu(1,1,3,e))
      fu(1,2,2,e) =
     &  0.25*(fu(1,2,1,e)+fu(1,3,2,e)+fu(1,2,3,e)+fu(1,1,2,e))
      do i=1,3
        do j=1,3        ! fill entire brick with surface solution to enable spectral interpolation
           fu(2,i,j,e)=fu(1,i,j,e)
           fu(3,i,j,e)=fu(1,i,j,e)
        enddo
      enddo
      endif

      if (pflmask(e).eq.5) then
      fu(1,1,1,e) = vpf(i_us+1)
      fu(3,1,1,e) = vpf(i_us+2)
      fu(1,3,1,e) = vpf(i_us+4)
      fu(3,3,1,e) = vpf(i_us+3)
      fu(2,1,1,e) = 0.5*(fu(1,1,1,e)+fu(3,1,1,e)) ! interpolate midpoints 
      fu(3,2,1,e) = 0.5*(fu(3,1,1,e)+fu(3,3,1,e))
      fu(2,3,1,e) = 0.5*(fu(1,3,1,e)+fu(3,3,1,e))
      fu(1,2,1,e) = 0.5*(fu(1,1,1,e)+fu(1,3,1,e))
      fu(2,2,1,e) =
     &  0.25*(fu(2,1,1,e)+fu(3,2,1,e)+fu(2,3,1,e)+fu(1,2,1,e))
      do i=1,3
        do j=1,3        ! fill entire brick with surface solution to enable spectral interpolation
           fu(i,j,2,e)=fu(i,j,1,e)
           fu(i,j,3,e)=fu(i,j,1,e)
        enddo
      enddo
      endif

      if (pflmask(e).eq.6) then
      fu(1,1,3,e) = vpf(i_us+1)
      fu(3,1,3,e) = vpf(i_us+2)
      fu(1,3,3,e) = vpf(i_us+4)
      fu(3,3,3,e) = vpf(i_us+3)
      fu(2,1,3,e) = 0.5*(fu(1,1,3,e)+fu(3,1,3,e)) ! interpolate midpoints 
      fu(3,2,3,e) = 0.5*(fu(3,1,3,e)+fu(3,3,3,e))
      fu(2,3,3,e) = 0.5*(fu(1,3,3,e)+fu(3,3,3,e))
      fu(1,2,3,e) = 0.5*(fu(1,1,3,e)+fu(1,3,3,e))
      fu(2,2,3,e) =
     &  0.25*(fu(2,1,3,e)+fu(3,2,3,e)+fu(2,3,3,e)+fu(1,2,3,e))
      do i=1,3
        do j=1,3        ! fill entire brick with surface solution to enable spectral interpolation
           fu(i,j,1,e)=fu(i,j,3,e)
           fu(i,j,2,e)=fu(i,j,3,e)
        enddo
      enddo
      endif

      endif
      enddo  

      return
      end
C=======================================================================
      subroutine int_surf12(fo,fi,n2,n1,ne)
      include 'SIZE'

      parameter(lx=20)
      real*8 z1(lx),z2(lx),w(lx)
      real*8 fm12(lx*lx),fm12t(lx*lx)
      real*8 work(lx*lx*lx)
      real*8 fo(n2,n2,n2,lelt), fi(n1,n1,n1,lelt)
      integer n1, n2, e, ne

      if (n1.gt.lx.or.n2.gt.lx) then
         write(6,*)'ERROR: increase lx in setmap to max:',n1,n2
         call exitt
      endif

      call rzero(fo,n2*n2*n2*ne)

      call zwgll(z1,w,n1)
      call zwgll(z2,w,n2)
      call igllm(fm12,fm12t,z1,z2,n1,n2,n1,n2)

      do e=1,ne
      call specmpf(fo(1,1,1,e),n2,fi(1,1,1,e),n1,fm12,fm12t,work)
      enddo

      return
      end
c-----------------------------------------------------------------------
      subroutine specmpf(b,nb,a,na,ba,ab,w)
C
C     -  Spectral interpolation from A to B via tensor products
C     -  scratch arrays: w(na*na*nb)
C
C
      include 'SIZE'
      include 'INPUT'
      real b(nb,nb,nb),a(na,na,na)
      real w(1)
C

         nab = na*nb
         nbb = nb*nb
         call mxm(ba,nb,a,na,b,na*na)
         k=1
         l=1
         do iz=1,na
            call mxm(b(k,1,1),nb,ab,na,w(l),nb)
            k=k+nab
            l=l+nbb
         enddo
         call mxm(w,nbb,ab,na,b,nb)
      return
      end
C=======================================================================
      subroutine flux_reconstruction()
      include 'SIZE'
      include 'TOTAL'
      include 'NEKMOOSE'
      common /flux_reconstructed/flux_recon(lx1,ly1,lz1,lelt)
      integer e,f,lx_low
      real*8  flux_low(3,3,3,lelt) 
      real*8  flux_recon
      ntot=nx1*ny1*nz1*nelt
!     Call to rassign and spectral interpolation

!     Testing on pc_f
      ntot=nw_bdt*4
      pc_f_max=glmax(pc_f,ntot)
      pc_f_min=glmin(pc_f,ntot) 

      call rassign(flux_low,pc_f)
!     Testing on flux_low
      ntot=nelt*3*3*3
      fl_max=glmax(flux_low,ntot)
      fl_min=glmin(flux_low,ntot)

      lx_low=3
      call int_surf12(flux_recon,flux_low,lx1,lx_low,lelt) 

      ntot=nelt*lx1*lx1*lx1
      fh_max=glmax(flux_recon,ntot)
      fh_min=glmin(flux_recon,ntot)

      if (nid.eq.0) then
        write(6,*)"> Min/Max flux 1: ",pc_f_min,pc_f_max
        write(6,*)"> Min/Max flux 2: ",fl_min,fl_max
        write(6,*)"> Min/Max flux 3: ",fh_min,fh_max
      endif


      sint1=0.0
      sarea1=0.0

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

      flux_moose1=sint1

      if (nid.eq.0) then
        write(6,*)"> Average reconstructed flux: ",flux_moose1
        write(6,*)"> Imposing flux: ",flux_moose
      endif

      ntot  = lx1*ly1*lz1*nelt
      scale=1.0
      if (abs(flux_moose1).gt.0.0) scale = flux_moose/flux_moose1 
      do i=1,ntot  
         flux_recon(i,1,1,1)=flux_recon(i,1,1,1)*scale
      enddo

c      call exitt()  

      return
      end
c-----------------------------------------------------------------------
