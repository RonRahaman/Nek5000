c-----------------------------------------------------------------------
      subroutine plan4_acc_data_copyin()
c-----------------------------------------------------------------------
      include 'SIZE'
      include 'TOTAL'    
      include 'GMRES'
      include 'HSMG'
c      include 'TSTEP'
c      include 'SOLN'

      parameter (lt=lx1*ly1*lz1*lelt)
      parameter (lwk=(lx1+2)*(ly1+2)*(lz1+2))
      common /hsmgw/ work(0:lwk-1),work2(0:lwk-1)

      real mg1(lt),mg2(lt),mg3(lt),mg4(lt),m2a(2*lt)
      common /scrmg/ mg1,mg2,mg3,mg4
      common /scrm2/ m2a

      real cg1(lt),cg2(lt),cg3(lt),cg4(lt)
      common /scrcg/ cg1,cg2,cg3,cg4

      real TA1 (lx1*ly1*lz1*lelv), 
     $   TA2 (lx1*ly1*lz1*lelt), 
     $   TA3 (lx1*ly1*lz1*lelt),
     $   TB1 (lx1*ly1*lz1*lelt),
     $   TB2 (lx1*ly1*lz1*lelt),
     $   TB3 (lx1*ly1*lz1*lelt),
     $   H2  (lx1*ly1*lz1*lelt)
      COMMON /SCRNS/ TA1, TA2, TA3, TB1, TB2, TB3, H2

      integer icalld
      save    icalld
      data    icalld/0/

      if (icalld.eq.0) then ! ONE TIME ONLY
        icalld=1
!$acc   enter data copyin (param)
!$acc   enter data copyin (ibc_acc)
!$acc   enter data copyin (cg1,cg2,cg3,cg4)
!$acc   enter data copyin (mg1,mg2,mg3,mg4)
!$acc   enter data copyin (m2a)
!$acc   enter data copyin (v1mask,v2mask,v3mask,pmask,tmask,omask)
!$acc   enter data copyin (pmult,tmult,vmult)
!$acc   enter data copyin (dxm1,dxtm1,w3m1)
!$acc   enter data copyin (bm1,binvm1,bintm1,bm1lag)
!$acc   enter data copyin (jacm1,jacmi)
!$acc   enter data copyin (xm1,ym1,zm1)
!$acc   enter data copyin (unx,uny,unz,area)
!$acc   enter data copyin (rxm1,sxm1,txm1)
!$acc   enter data copyin (rym1,sym1,tym1)
!$acc   enter data copyin (rzm1,szm1,tzm1)
!$acc   enter data copyin (g1m1,g2m1,g3m1,g4m1,g5m1,g6m1)
!$acc   enter data copyin (cbc,bc)
!$acc   enter data copyin(
!$acc&     vxlag,vylag,vzlag,tlag,vgradt1,vgradt2
!$acc&    ,abx1,aby1,abz1,abx2,aby2,abz2,vdiff_e
!$acc&    ,vx,vy,vz,vx_e,vy_e,vz_e,t,vtrans,vdiff
!$acc&    ,bfx,bfy,bfz
!$acc&    ,bq,pr,prlag,qtl,usrdiv)

!$acc   enter data copyin (mg_mask,mg_imask,pmask)
!$acc   enter data copyin (mg_jht,mg_jh,mg_rstr_wt,mg_schwarz_wt)
!$acc   enter data copyin (mg_work,mg_fast_s,mg_fast_d)
!$acc   enter data copyin (h_gmres,w_gmres,v_gmres,z_gmres)
!$acc   enter data copyin (c_gmres,s_gmres,x_gmres,gamma_gmres,wk_gmres)
!$acc   enter data copyin (r_gmres)
!$acc   enter data copyin (ml_gmres,mu_gmres)
!$acc   enter data copyin (ta1,ta2,ta3,tb1,tb2,tb3,h2)
!$acc   enter data copyin (abx1,abx2,aby1,aby2,abz1,abz2)
!$acc   enter data copyin (bd)

      endif

!$acc enter data copyin (c_vx)

      return
      end
c-----------------------------------------------------------------------
      subroutine print_acc(a,n,text,mode)
c-----------------------------------------------------------------------
      include 'SIZE'
      integer n,i,mode
      real    a(n)
      character*4 text

!$ACC update host(a) if (mode.eq.1)  !copies the data from device to host
      if (nid.eq.0) then
!         do i=1,n
!            write(6,10) text,i,a(i)
!         enddo
! 10      format(a4,i300,1p1e15.4)
          write(6,102) text, (a(i), i=1,4)
      endif
  102    format(a4,2x,1p4e15.4)

      return
      end
c-----------------------------------------------------------------------
      subroutine print_acc_int(a,n,text,mode)
c-----------------------------------------------------------------------
      include 'SIZE'
      integer n,i,mode
      integer a(n)
      character*4 text

!$ACC update host(a) if (mode.eq.1)  !copies the data from device to !host
      if (nid.eq.0) write(6,*) text,(a(i),i=1,n) ! prints data on host

      return
      end
c-----------------------------------------------------------------------
      subroutine global_grad3(d,u,u1,u2,u3)
c-----------------------------------------------------------------------

      include 'SIZE'
      integer i,j,k,l,e
      real d (lx1,lx1)
      real u (lx1,ly1,lz1,lelt)
      real u1(lx1,ly1,lz1,lelt)
      real u2(lx1,ly1,lz1,lelt)
      real u3(lx1,ly1,lz1,lelt)
      real tmpu1,tmpu2,tmpu3

!$ACC DATA PRESENT (d(nx1,nx1))
!$ACC&     PRESENT (u(nx1,ny1,nz1,nelt))
!$ACC&     PRESENT (u1(nx1,ny1,nz1,nelt),u2(nx1,ny1,nz1,nelt))
!$ACC&     PRESENT (u3(nx1,ny1,nz1,nelt))
!$ACC PARALLEL LOOP COLLAPSE(4) GANG WORKER VECTOR
!$ACC&    PRIVATE(tmpu1,tmpu2,tmpu3)
!dir$ NOBLOCKING
      do e=1,nelt
         do k=1,nz1
            do j=1,ny1
               do i=1,nx1
                  tmpu1 = 0.0
                  tmpu2 = 0.0
                  tmpu3 = 0.0
!$ACC LOOP SEQ
                  do l=1,nx1
                     tmpu1 = tmpu1 + d(i,l)*u(l,j,k,e)
                     tmpu2 = tmpu2 + d(j,l)*u(i,l,k,e)
                     tmpu3 = tmpu3 + d(k,l)*u(i,j,l,e)
                  enddo
                  u1(i,j,k,e) = tmpu1
                  u2(i,j,k,e) = tmpu2
                  u3(i,j,k,e) = tmpu3
               enddo
            enddo
         enddo
      enddo
!$ACC END LOOP
!$ACC END DATA
      return
      end
c-----------------------------------------------------------------------
      subroutine global_div3(d,u1,u2,u3,v1,v2,v3)
c-----------------------------------------------------------------------

      include 'SIZE'
      integer i,j,k,l,e
      real d (lx1,ly1)
      real u1(lx1,ly1,lz1,lelt)
      real u2(lx1,ly1,lz1,lelt)
      real u3(lx1,ly1,lz1,lelt)
      real v1(lx1,ly1,lz1,lelt)
      real v2(lx1,ly1,lz1,lelt)
      real v3(lx1,ly1,lz1,lelt)
      real tmpu1,tmpu2,tmpu3

!$ACC DATA PRESENT (d (nx1,nx1))
!$ACC&     PRESENT (u1(nx1,ny1,nz1,nelt))
!$ACC&     PRESENT (u2(nx1,ny1,nz1,nelt),u3(nx1,ny1,nz1,nelt))
!$ACC$     PRESENT (v1(nx1,ny1,nz1,nelt),v2(nx1,ny1,nz1,nelt))
!$ACC&     PRESENT (v3(nx1,ny1,nz1,nelt))
!$ACC PARALLEL LOOP COLLAPSE(4) GANG WORKER VECTOR
!$ACC&    PRIVATE(tmpu1,tmpu2,tmpu3)
!dir$ NOBLOCKING
      do e=1,nelt
         do k=1,nz1
            do j=1,ny1
               do i=1,nx1
                  tmpu1 = 0.0
                  tmpu2 = 0.0
                  tmpu3 = 0.0
!$ACC LOOP SEQ
                  do l=1,nx1
                     tmpu1 = tmpu1 + d(i,l)*u1(l,j,k,e)
                     tmpu2 = tmpu2 + d(j,l)*u2(i,l,k,e)
                     tmpu3 = tmpu3 + d(k,l)*u3(i,j,l,e)
                  enddo
                  v1(i,j,k,e) = tmpu1
                  v2(i,j,k,e) = tmpu2
                  v3(i,j,k,e) = tmpu3
               enddo
            enddo
         enddo
      enddo
!$ACC END LOOP
!$ACC END DATA
      end
c-----------------------------------------------------------------------
      subroutine makef_acc
c
c     Compute and add: (1) user specified forcing function (FX,FY,FZ)
c                      (2) driving force due to natural convection
c                      (3) convection term
c
c     !! NOTE: Do not change the arrays BFX, BFY, BFZ until the
c              current time step is completed.
c
      include 'SIZE'
      include 'SOLN'
      include 'MASS'
      include 'INPUT'
      include 'TSTEP'

      include 'MVGEOM'

      call chck('abb')
      call makeuf_acc    ! paul and som
      call chck('bbb')
      call advab_acc
      call chck('cbb')
      call makeabf_acc
      call chck('dbb')
      call makebdf_acc
      call chck('ebb')

      return
      end
c-----------------------------------------------------------------------
      subroutine advab_acc
C---------------------------------------------------------------
C
C     Eulerian scheme, add convection term to forcing function 
C     at current time step.
C
C---------------------------------------------------------------
      include 'SIZE'
      include 'SOLN'
      include 'MASS'
      include 'TSTEP'

      real ta1 (lx1*ly1*lz1*lelv)
     $   , ta2 (lx1*ly1*lz1*lelv)
     $   , ta3 (lx1*ly1*lz1*lelv)


!$acc data create(ta1,ta2,ta3)
!$acc&     present(vx,vy,vz,bfx,bfy,bfz,bm1)

      n = lx1*ly1*lz1*nelv

      call chck('a11')
      call convop_acc  (ta1,vx)
      call chck('b11')
      call convop_acc  (ta2,vy)
      call chck('c11')
      call convop_acc  (ta3,vz)
      call chck('d11')

!$acc parallel loop 
      do i=1,n
         bfx(i,1,1,1)=bfx(i,1,1,1)-ta1(i)*bm1(i,1,1,1)
         bfy(i,1,1,1)=bfy(i,1,1,1)-ta2(i)*bm1(i,1,1,1)
         bfz(i,1,1,1)=bfz(i,1,1,1)-ta3(i)*bm1(i,1,1,1)
      enddo
!$acc end loop
!$acc end data 

      return
      end
c-----------------------------------------------------------------------
      subroutine makebdf_acc
C
C     Add contributions to F from lagged BD terms.
C
      include 'SIZE'
      include 'SOLN'
      include 'MASS'
      include 'GEOM'
      include 'INPUT'
      include 'TSTEP'
C
      COMMON /SCRNS/ TA1(LX1*LY1*LZ1*LELV)
     $ ,             TA2(LX1*LY1*LZ1*LELV)
     $ ,             TA3(LX1*LY1*LZ1*LELV)
     $ ,             TB1(LX1*LY1*LZ1*LELV)
     $ ,             TB2(LX1*LY1*LZ1*LELV)
     $ ,             TB3(LX1*LY1*LZ1*LELV)
     $ ,             H2 (LX1*LY1*LZ1*LELV)
C
      NTOT1 = NX1*NY1*NZ1*NELV
      CONST = 1./DT

      if(iflomach) then
        call cfill_acc(h2,CONST,ntot1)
      else
        call cmult2_acc(h2,vtrans(1,1,1,1,ifield),const,ntot1)
      endif

      call chck('mma')
      CALL opcolv3c_acc (TB1,TB2,TB3,VX,VY,VZ,BM1,bd(2))
      call chck('mmb')
C
      DO 100 ILAG=2,NBD
         call chck('mmc')
         IF (IFGEOM) THEN
            CALL opcolv3c_acc(TA1,TA2,TA3,VXLAG (1,1,1,1,ILAG-1),
     $                                VYLAG (1,1,1,1,ILAG-1),
     $                                VZLAG (1,1,1,1,ILAG-1),
     $                                BM1LAG(1,1,1,1,ILAG-1),bd(ilag+1))
         ELSE
            CALL opcolv3c_acc(TA1,TA2,TA3,VXLAG (1,1,1,1,ILAG-1),
     $                                VYLAG (1,1,1,1,ILAG-1),
     $                                VZLAG (1,1,1,1,ILAG-1),
     $                                BM1                   ,bd(ilag+1))
         ENDIF
         call chck('mmd')
         CALL OPADD2_ACC  (TB1,TB2,TB3,TA1,TA2,TA3)
         call chck('mme')
 100  CONTINUE
      call chck('mmf')
      CALL opadd2col_acc (BFX,BFY,BFZ,TB1,TB2,TB3,h2)
      call chck('mmg')
C
      return
      END
c-----------------------------------------------------------------------
      subroutine makeabf_acc
C-----------------------------------------------------------------------
C
C     Sum up contributions to kth order extrapolation scheme.
C
C-----------------------------------------------------------------------
      include 'SIZE'
      include 'INPUT'
      include 'SOLN'
      include 'TSTEP'
C
      COMMON /SCRNS/ TA1 (LX1*LY1*LZ1*LELT)
     $ ,             TA2 (LX1*LY1*LZ1*LELT)
     $ ,             TA3 (LX1*LY1*LZ1*LELT)
C
      NTOT1 = LX1*LY1*LZ1*LELT
C
      AB0 = AB(1)
      AB1 = AB(2)
      AB2 = AB(3)
      CALL ADD3S2_ACC (TA1,ABX1,ABX2,AB1,AB2,NTOT1)
      CALL ADD3S2_ACC (TA2,ABY1,ABY2,AB1,AB2,NTOT1)
      CALL COPY_ACC   (ABX2,ABX1,NTOT1)
      CALL COPY_ACC   (ABY2,ABY1,NTOT1)
      CALL COPY_ACC   (ABX1,BFX,NTOT1)
      CALL COPY_ACC   (ABY1,BFY,NTOT1)
      CALL ADD2S1_ACC (BFX,TA1,AB0,NTOT1)
      CALL ADD2S1_ACC (BFY,TA2,AB0,NTOT1)
      CALL ADD3S2_ACC (TA3,ABZ1,ABZ2,AB1,AB2,NTOT1)
      CALL COPY_ACC   (ABZ2,ABZ1,NTOT1)
      CALL COPY_ACC   (ABZ1,BFZ,NTOT1)
      CALL ADD2S1_ACC (BFZ,TA3,AB0,NTOT1)
      if(.not.iflomach) CALL COL2_ACC (BFX,VTRANS,NTOT1) ! multiply by density
      if(.not.iflomach) CALL COL2_ACC (BFY,VTRANS,NTOT1)
      if(.not.iflomach) CALL COL2_ACC (BFZ,VTRANS,NTOT1)
C
      return
      END
C
c-----------------------------------------------------------------------
      subroutine makeuf_acc
C---------------------------------------------------------------------
C
C     Compute and add: (1) user specified forcing function (FX,FY,FZ)
C     for the gpu version of the code, FX=FY=FZ=0  
C
C----------------------------------------------------------------------
      include 'SIZE'
      include 'SOLN'
      include 'MASS'
      include 'TSTEP'

      time = time-dt
      call nekuf       (bfx,bfy,bfz)

!$acc update device    (bfx,bfy,bfz)
      call opcolv_acc  (bfx,bfy,bfz,bm1)

      call chk2('bfx',bfx)
      call chk2('bfy',bfy)
      call chk2('bfz',bfz)

      time = time+dt

      return
      end
c-----------------------------------------------------------------------
      subroutine oprzero_acc (a,b,c)
      include 'SIZE'
      real a(1),b(1),c(1)

      ntot1=nx1*ny1*nz1*nelv

      call rzero_acc(a,ntot1)
      call rzero_acc(b,ntot1)
      call rzero_acc(c,ntot1)

      return
      end
c------------------------------------------------------------------------
      subroutine opcolv_acc (a1,a2,a3,c)
      include 'SIZE'
      real a1(1),a2(1),a3(1),c(1)

      n=nx1*ny1*nz1*nelv

!$acc data present(a1,a2,a3,c)
!$acc    parallel loop
         do i=1,n
            a1(i)=a1(i)*c(i)
            a2(i)=a2(i)*c(i)
            a3(i)=a3(i)*c(i)
         enddo
!$acc    end parallel loop
!$acc end data

      return
      end
C------------------------------------------------------------------------
      subroutine get_jggl(jggl,dggl) ! acc copyin happens here
      include 'SIZE'

      real jggl(lxd,lx1),dggl(lxd,lx1)


      parameter (ldg=lxd**3,lwkd=4*lxd*lxd)
      common /dgrad/ d(ldg),dt(ldg),dg(ldg),dgt(ldg),jgl(ldg),jgt(ldg)
     $             , wkd(lwkd)
      real jgl,jgt

      call get_int_ptr (i,nx1,nxd)
      call copy(jggl,jgl(i),lx1*lxd)          ! jggl = jgl

      call get_dgl_ptr (ip,nxd,nxd)
      call mxm(dg(ip),nxd,jggl,nxd,dggl,nx1)  ! dggl = dg*jggl


      return
      end
c-----------------------------------------------------------------------
      subroutine convop_acc(du,u)

      include 'SIZE'
      include 'TOTAL'
      include 'CTIMER'    ! Contains icalld

      real    du (lx1,ly1,lz1,lelt)
      real    u  (lx1,ly1,lz1,lelt)

      real jggl(lxd,lx1),dggl(lxd,lx1)
      save jggl,dggl

      call chck('q11')
      if (icalld.eq.0) then
         tadvc=0.0
         call get_jggl(jggl,dggl) ! acc copyin happens here
      call chck('q21')
!$acc    enter data copyin(jggl,dggl)
      call chck('q31')
      endif

      icalld=icalld+1
      nadvc=icalld
      etime1=dnekclock()

      call chck('q41')
      call convop_fst_3d_acc(du,u,c_vx,bm1,jggl,dggl)  
      call chck('q51')

      tadvc=tadvc+(dnekclock()-etime1)

      return
      END
c-------------------------------------------------------------------
      subroutine convop_fst_3d_acc(du,u,c,b,jj,dd)
c
      include 'SIZE'
c
c     apply convecting field c to scalar field u
c
c           T
c     du = J   ( C . grad Ju)
c

      real du(lx1,ly1,lz1,lelt)  , u(lx1,ly1,lz1,lelt)
      real  c(lxd*lyd*lzd,lelv,3), b(lx1,ly1,lz1,lelt)

      real jj(lxd,lx1)
      real dd(lxd,lx1)

      real wk1(lxd,ly1,lz1),wk2(lxd,ly1,lz1)
      real wk3(lxd,lyd,lz1),wk4(lxd,lyd,lz1),wk5(lxd,lyd,lz1)
      real wk6(lxd,lyd,lzd)

      integer e,p,q,r

      call chck('p11')
      write(6,*) nelv,' echk'

!$acc parallel loop present(du,u,c,b,jj,dd) gang
!$acc&              private(wk1,wk2,wk3,wk4,wk5,wk6)
      do e=1,nelv

!$acc    loop collapse(2) vector
         do j=1,ly1*lz1
         do i=1,lxd
            w1 = 0.0
            w2 = 0.0
!$acc       loop seq
            do p=1,lx1
               w1 = w1 + dd(i,p) * u(p,j,1,e) ! Grad:  crs->fine
               w2 = w2 + jj(i,p) * u(p,j,1,e) ! Inter: crs->fine
            enddo
!$acc       end loop
            wk1(i,j,1)=w1
            wk2(i,j,1)=w2
         enddo
         enddo
!$acc    end loop



!$acc    loop collapse(3) vector
         do k=1,lz1
         do i=1,lxd
         do j=1,lyd
            w1 = 0.0
            w2 = 0.0
            w3 = 0.0
!$acc       loop seq
            do q=1,ly1
               w1 = w1 + jj(j,q) * wk1(i,q,k) ! JxD u
               w2 = w2 + dd(j,q) * wk2(i,q,k) ! DxJ u
               w3 = w3 + jj(j,q) * wk2(i,q,k) ! JxJ u
            enddo
!$acc       end loop
            wk3(i,j,k)=w1
            wk4(i,j,k)=w2
            wk5(i,j,k)=w3
         enddo
         enddo
         enddo
!$acc    end loop


         l=0
!$acc    loop collapse(2) vector
         do i=1,lxd*lyd
         do k=1,lzd
            w1 = 0.0
            w2 = 0.0
            w3 = 0.0
!$acc       loop seq
            do r=1,lz1
               w1 = w1 + jj(k,r) * wk3(i,1,r) ! JxJxD u
               w2 = w2 + jj(k,r) * wk4(i,1,r) ! JxDxJ u
               w3 = w3 + dd(k,r) * wk5(i,1,r) ! DxJxJ u
            enddo
!$acc       end loop
            l=l+1
            wk6(i,1,k)=w1*c(l,e,1)+w2*c(l,e,2)+w3*c(l,e,3) ! c1*ur+c2*us+c3*ut
         enddo
         enddo
!$acc    end loop


!! START COLLAPSING BACK with J'


!$acc    loop collapse(2) vector
         do i=1,lxd*lyd
         do k=1,lz1
            w1 = 0.0
!$acc       loop seq
            do r=1,lzd                        !  T
               w1 = w1 + jj(r,k) * wk6(i,1,r) ! J  x I x I w6
            enddo
!$acc       end loop
            wk5(i,1,k)=w1
         enddo
         enddo
!$acc    end loop


!$acc    loop collapse(3) vector
         do k=1,lz1
         do i=1,lxd
         do j=1,ly1
            w1 = 0.0
!$acc       loop seq
            do q=1,lyd
               w1 = w1 + jj(q,j) * wk5(i,q,k)
            enddo
!$acc       end loop
            wk2(i,j,k)=w1
         enddo
         enddo
         enddo
!$acc    end loop


!$acc    loop collapse(2) vector
         do j=1,ly1*lz1
         do i=1,lx1
            w1 = 0.0
!$acc       loop seq
            do p=1,lxd
               w1 = w1 + jj(p,i) * wk2(p,j,1)
            enddo
!$acc       end loop
            du(i,j,1,e) = w1*b(i,j,1,e)
         enddo
         enddo
!$acc    end loop


      enddo
!$acc end parallel loop
      return
      end
c----------------------------------------------------------------------
      subroutine plan4_acc (igeom)

c     Splitting scheme A.G. Tomboulides et al.
c     Journal of Sci.Comp.,Vol. 12, No. 2, 1998

C     NOTE: QTL denotes the so called thermal
c           divergence and has to be provided
c           by userqtl.

      include 'SIZE'
      include 'TOTAL'
      include 'CTIMER'

      parameter(lt=lx1*ly1*lz1*lelv)
      real res1(lt),res2(lt),res3(lt)
     $   , dv1 (lt),dv2(lt) ,dv3(lt)
     $   , h1  (lt),h2 (lt) ,respr(lt)
     $   , dpr (lt)
 
      logical ifstsp

!$acc data create(res1,res2,res3,dv1,dv2,dv3,h1,h2,respr,dpr)

      if (icalld.eq.0) tpres=0.0
      icalld=icalld+1
      npres=icalld

      intype = -1
      n      = nx1*ny1*nz1*nelv


      if (igeom.eq.1) then  !  Setup RHS (on geometry at time t^n-1)

         call chck('aaa')
         call plan4_acc_data_copyin()
         call chck('baa')
         call makef_acc     ! explicit contributions bfx,bfy,bfz 
         call chck('caa')
         call print_acc(bfx,3,'bfx ',1)
         call print_acc(bfy,3,'bfy ',1)
         call print_acc(bfz,3,'bfz ',1)
         !call chk2('caa',bfx)
         !call chk2('ca2',bfy)
         !call chk2('ca3',bfz)
         call sumab_vx_acc  ! Extrapolate velocity
         call chck('daa')

      else                  ! Solve system (on geometry at time t^n)

         call chck('eaa')
         call add2_acc (qtl,usrdiv,n) ! add user defd div. to qtl 
         call chck('faa')
         call lagvel_acc
         call chck('gaa')
         call bcdirvc_acc (vx,vy,vz,v1mask,v2mask,v3mask) ! Dirichlet 
         call chck('haa')

         call crespsp_acc  (respr)
         call chk2('xyz',xm1)
         call chk2('iaa',respr)
         call invers2_acc  (h1,vtrans,n)
         call chck('jaa')
         call rzero_acc    (h2,n)
         call chck('kaa')
         call ctolspl_acc  (tolspl,respr)
         call chck('laa')
         call print_acc(h1,3,'h1  ',1)
         call print_acc(h2,3,'h2  ',1)
         !call chk2('h1x',h1)
         !call chk2('h2x',h2)

         call chck('maa')
         etime1=dnekclock()
         call chck('naa')
         call copy_acc     (dpr,respr,n)
         call print_acc(respr,3,'res ',1)
         !call chk2('oaa',respr)

         call hmh_gmres_acc(dpr,h1,h2,vmult,nmxh)   ! compute pressure

         call chck('paa')
         call add2_acc     (pr,dpr,n)
         call chck('qaa')
         call ortho_acc    (pr)
         call chck('raa')
         tpres=tpres+(dnekclock()-etime1)

         call chck('saa')
         call cresvsp_acc (res1,res2,res3,h1,h2) !  compute velocity
         call chck('taa')
         call ophinv_pr_acc(dv1,dv2,dv3,res1,res2,res3,h1,h2,tolhv,nmxh)
         call chck('uaa')
         call add2_acc    (vx,dv1,n)      
         call chck('vaa')
         call add2_acc    (vy,dv2,n)
         call chck('waa')
         call add2_acc    (vz,dv3,n)
         call chck('xaa')
 
         call chck('yaa')
      endif
!$acc end data
 
      return
      end
c-----------------------------------------------------------------------
      subroutine crespsp_acc (respr)

C     Compute startresidual/right-hand-side in the pressure

      include 'SIZE'
      include 'TOTAL'

      real           respr (lx1,ly1,lz1,lelv)
c
      common /scrns/ ta1   (lx1,ly1,lz1,lelv)
     $ ,             ta2   (lx1,ly1,lz1,lelv)
     $ ,             ta3   (lx1,ly1,lz1,lelv)
     $ ,             wa1   (lx1*ly1*lz1*lelv)
     $ ,             wa2   (lx1*ly1*lz1*lelv)
     $ ,             wa3   (lx1*ly1*lz1*lelv)
      common /scrmg/ w1    (lx1,ly1,lz1)
     $ ,             w2    (lx1,ly1,lz1)
     $ ,             w3    (lx1,ly1,lz1)

      character cb*3,c1*1
      integer e,f

!$acc routine(facind) seq
      
      nxyz1  = nx1*ny1*nz1
      ntot1  = nxyz1*nelv
      nfaces = 2*ndim

      write(6,*) 'call opcurl'

c     -mu*curl(curl(v))
      call op_curl (ta1,ta2,ta3,vx_e,vy_e,vz_e,.true.,w1,w2)
      call op_curl (wa1,wa2,wa3,ta1,ta2,ta3,.true.,w1,w2)

      call opgrad  (ta1,ta2,ta3,qtl)

      call opcolv  (wa1,wa2,wa3,bm1)
      scale = -4./3. 
      call opadd2cm(wa1,wa2,wa3,ta1,ta2,ta3,scale)

c compute stress tensor for ifstrs formulation - variable viscosity Pn-Pn
      if (ifstrs) 
     $   call exitti('ifstrs not yet support on gpu$',nelv)

      call invcol3  (w1,vdiff,vtrans,ntot1)
      call opcolv   (wa1,wa2,wa3,w1)

c     add old pressure term because we solve for delta p 

      call chck('a99')

!$acc enter data copyin(ta1,ta2,ta3,wa1,wa2,wa3)

      call chk2('b91',ta1)
      call chk2('b92',ta2)
      call chk2('b93',ta3)

      call chk2('c91',wa1)
      call chk2('c92',wa2)
      call chk2('c93',wa3)


      call invers2_acc (ta1,vtrans,ntot1)
      call chck('c99')
      call rzero_acc   (ta2,ntot1)
      call chck('d99')

      call bcdirsc_acc (pr)
      call chck('e99')

      call chck('f99')
      call axhelm_acc  (respr,pr,ta1,ta2,1,1)
      call chck('g99')
      call chsign_acc  (respr,ntot1)
      call chck('h99')

c     add explicit (NONLINEAR) terms 
      n = nx1*ny1*nz1*nelv
!$acc parallel loop present(ta1,ta2,ta3,bfx,bfy,bfz,vtrans,wa1,wa2,wa3)
      do i=1,n
         ta1(i,1,1,1) = bfx(i,1,1,1)/vtrans(i,1,1,1,1)-wa1(i)
         ta2(i,1,1,1) = bfy(i,1,1,1)/vtrans(i,1,1,1,1)-wa2(i)
         ta3(i,1,1,1) = bfz(i,1,1,1)/vtrans(i,1,1,1,1)-wa3(i)
      enddo
!$acc end parallel

      write(6,*) 'call dssum',ifield
      call dssum (ta1,nx1,ny1,nz1)
      call dssum (ta2,nx1,ny1,nz1)
      call dssum (ta3,nx1,ny1,nz1)
      write(6,*) 'done dssum',ifield

!$acc parallel loop present(ta1,ta2,ta3,binvm1)
      do i=1,n
         ta1(i,1,1,1) = ta1(i,1,1,1)*binvm1(i,1,1,1)
         ta2(i,1,1,1) = ta2(i,1,1,1)*binvm1(i,1,1,1)
         ta3(i,1,1,1) = ta3(i,1,1,1)*binvm1(i,1,1,1)
      enddo
!$acc end parallel
      call chck('h79')


C     ADD SURFACE TERMS (now, to free up ta3)

      call izero(ibc_acc,nfaces*nelv)
      do e=1,nelv
      do f=1,nfaces
         cb = cbc(f,e,ifield)
         c1 = cbc(f,e,ifield)
         if (c1.eq.'v'.or.c1.eq.'V'.or.
     $       cb.eq.'MV '.or.cb.eq.'mv ') ibc_acc(f,e)=1
         if (cb.eq.'SYM')                ibc_acc(f,e)=2
      enddo
      enddo
!$acc update device(ibc_acc)
      

!$acc parallel loop collapse(2) gang
!$acc&present(vx,vy,vz,ta1,ta2,ta3,unx,uny,unz,area,respr,cbc,bc)
      do e=1,nelv
      do f=1,nfaces
         i0=1
         j0=1
         k0=1
         i1=lx1
         j1=ly1
         k1=lz1
         if (f.eq.1) j1=1
         if (f.eq.2) i0=lx1
         if (f.eq.3) j0=ly1
         if (f.eq.4) i1=1
         if (f.eq.5) k1=1
         if (f.eq.6) k0=lz1

         if (ibc_acc(f,e).eq.1) then
            l=0
!$acc       loop vector collapse(3)
            do k=k0,k1
            do j=j0,j1
            do i=i0,i1
               l=l+1
               tmp    =(vx(i,j,k,e)*unx(l,1,f,e)
     $                 +vy(i,j,k,e)*uny(l,1,f,e)
     $                 +vz(i,j,k,e)*unz(l,1,f,e))*area(l,1,f,e)
     $                 *dtdb
               respr(i,j,k,e)=respr(i,j,k,e)-tmp
            enddo
            enddo
            enddo
         elseif (ibc_acc(f,e).eq.2) then
            l=0
!$acc       loop vector collapse(3)
            do k=k0,k1
            do j=j0,j1
            do i=i0,i1
               l=l+1
               tmp    =(ta1(i,j,k,e)*unx(l,1,f,e)
     $                 +ta2(i,j,k,e)*uny(l,1,f,e)
     $                 +ta3(i,j,k,e)*unz(l,1,f,e))*area(l,1,f,e)
               respr(i,j,k,e)=respr(i,j,k,e)-tmp
            enddo
            enddo
            enddo
         endif
      enddo
      enddo
!$acc end parallel
      call chck('h89')

!$acc parallel loop gang
!$acc&private(w1,w2,w3)
!$acc&present(ta1,ta2,ta3,w3m1,rxm1,rym1,rzm1
!$acc&         ,sxm1,sym1,szm1,txm1,tym1,tzm1
!$acc&         ,dxtm1,bm1,qtl,respr)
      do e=1,nelv
!$acc  loop vector
       do i=1,lx1*ly1*lz1
         w1(i,1,1) = (rxm1(i,1,1,e)*ta1(i,1,1,e) ! Jacobian
     $               +rym1(i,1,1,e)*ta2(i,1,1,e) ! included
     $               +rzm1(i,1,1,e)*ta3(i,1,1,e))*w3m1(i,1,1)
         w2(i,1,1) = (sxm1(i,1,1,e)*ta1(i,1,1,e)
     $               +sym1(i,1,1,e)*ta2(i,1,1,e)
     $               +szm1(i,1,1,e)*ta3(i,1,1,e))*w3m1(i,1,1)
         w3(i,1,1) = (txm1(i,1,1,e)*ta1(i,1,1,e)
     $               +tym1(i,1,1,e)*ta2(i,1,1,e)
     $               +tzm1(i,1,1,e)*ta3(i,1,1,e))*w3m1(i,1,1)
       enddo

!$acc  loop vector collapse(3)
       do k=1,nz1
       do j=1,ny1
       do i=1,nx1
          t1 = 0.0
!$acc     loop seq
          do l=1,nx1
             t1 = t1 + dxtm1(i,l)*w1(l,j,k) ! D^T
     $               + dxtm1(j,l)*w2(i,l,k)
     $               + dxtm1(k,l)*w3(i,j,l)
          enddo
          respr(i,j,k,e) = respr(i,j,k,e) + t1
     $                   + dtbd*bm1(i,j,k,e)*qtl(i,j,k,e)
       enddo
       enddo
       enddo

      enddo
!$acc end parallel

C     Orthogonalize to (1,1,...,1)T for all-Dirichlet case
      call ortho_acc (respr)

!$acc exit data

      return
      end
c----------------------------------------------------------------------
      subroutine bcdirsc_acc(s)
C
C     Apply Dirichlet boundary conditions to surface of scalar, S.
C     Use IFIELD as a guide to which boundary conditions are to be applied.
C
      include 'SIZE'
      include 'TSTEP'
      include 'INPUT'
      include 'SOLN'
      include 'TOPOL'
      include 'CTIMER'
C
      real s(lx1,ly1,lz1,lelt)
      common /scrsf/ tmp(lx1,ly1,lz1,lelt)
     $             , tma(lx1,ly1,lz1,lelt)
     $             , smu(lx1,ly1,lz1,lelt)
      common  /nekcb/ cb
      character cb*3
      integer e,f

      if (icalld.eq.0) then
         tusbc=0.0
         nusbc=0
         icalld=icalld+1
      endif
      nusbc=nusbc+1
      etime1=dnekclock()
C
      ifld   = 1
      nfaces = 2*ndim
      nxyz   = nx1*ny1*nz1
      nel    = nelfld(ifield)
      ntot   = nxyz*nel
      nfldt  = nfield - 1

      write(6,*) 'BCDIRSC NOT READY'
      return

      call rzero(tmp,ntot)

C     Temperature/pressure boundary condition

      do isweep=1,2

         do e=1,nel
         do f=1,nfaces
            cb=cbc(iface,ie,ifield)
            bc1=bc(1,iface,ie,ifield)
            bc2=bc(2,iface,ie,ifield)
            bc3=bc(3,iface,ie,ifield)
            bc4=bc(4,iface,ie,ifield)
            bck=bc(4,iface,ie,ifld)
            bce=bc(5,iface,ie,ifld)
            if (cb.eq.'T  ') call facev (tmp,e,f,bc1,nx1,ny1,nz1)
            if (cb.eq.'MCI') call facev (tmp,e,f,bc4,nx1,ny1,nz1)
            if (cb.eq.'MLI') call facev (tmp,e,f,bc4,nx1,ny1,nz1)
            if (cb.eq.'KD ') call facev (tmp,e,f,bck,nx1,ny1,nz1)
            if (cb.eq.'ED ') call facev (tmp,e,f,bce,nx1,ny1,nz1)
            if (cb.eq.'t  ' .or. cb.eq.'kd ' .or.
     $          cb.eq.'ed ' .or. cb.eq.'o  ' .or. cb.eq.'on ') 
     $          call faceis (cb,tmp(1,1,1,e),e,f,nx1,ny1,nz1)
         enddo
         enddo

c        Take care of Neumann-Dirichlet shared edges...

         if (isweep.eq.1) call dsop(tmp,'mxa',nx1,ny1,nz1)
         if (isweep.eq.2) call dsop(tmp,'mna',nx1,ny1,nz1)
      enddo
C
C     Copy temporary array to temperature array.
C
      if (ifield.eq.1) then
         call col2_acc(s,pmask,ntot)
      else
         call col2_acc(s,tmask(1,1,1,1,ifield-1),ntot)
      endif
      call add2_acc(s,tmp,ntot)

      tusbc=tusbc+(dnekclock()-etime1)

      return
      end
c-----------------------------------------------------------------------
      subroutine cresvsp_acc (resv1,resv2,resv3,h1,h2)

C     Compute the residual for the velocity

      include 'SIZE'
      include 'TOTAL'

      real resv1(lx1*ly1*lz1*lelv)
     $   , resv2(lx1*ly1*lz1*lelv)
     $   , resv3(lx1*ly1*lz1*lelv)
     $   , h1   (lx1*ly1*lz1*lelv)
     $   , h2   (lx1*ly1*lz1*lelv)

      common /scruz/ ta1   (lx1*ly1*lz1*lelv)
     $ ,             ta2   (lx1*ly1*lz1*lelv)
     $ ,             ta3   (lx1*ly1*lz1*lelv)
     $ ,             ta4   (lx1*ly1*lz1*lelv)

      n = lx1*ly1*lz1*nelv
      intype = -1

      scale = -1./3.
      if (ifstrs) scale =  2./3.

!$acc data present_or_create(ta1,ta2,ta3,ta4)
!$acc&     present(resv1,resv2,resv3,h1,h2,qtl,pr,vdiff,vtrans)
!$acc&     present(bfx,bfy,bfz,vx,vy,vz)

      call chk2('lp1',pr)
!$acc parallel loop 
      do i=1,n
         ta4(i)=vdiff (i,1,1,1,1)*qtl(i,1,1,1)+scale*pr(i,1,1,1)
         h1 (i)=vdiff (i,1,1,1,1)
         h2 (i)=vtrans(i,1,1,1,1)*dtbd
      enddo
      call chk2('lp2',pr)

      call wgradm1_acc  (ta1,ta2,ta3,ta4)
      call chk2('lp3',pr)
      call ophx_acc     (resv1,resv2,resv3,vx,vy,vz,h1,h2)
      call chk2('lp4',pr)

!$acc parallel loop
      do i=1,n
         resv1(i)=bfx(i,1,1,1)-resv1(i)-ta1(i)
         resv2(i)=bfy(i,1,1,1)-resv2(i)-ta2(i)
         resv3(i)=bfz(i,1,1,1)-resv3(i)-ta3(i)
      enddo
      call chk2('lp5',pr)

!$acc end data

      return
      end
c-----------------------------------------------------------------------
      subroutine ophx_acc (out1,out2,out3,inp1,inp2,inp3,h1,h2)

c     OUT = (H1*A+H2*B) * INP  

      include 'SIZE'
      include 'INPUT'
      include 'SOLN'
      real out1(1),out2(1),out3(1),inp1(1),inp2(1),inp3(1),h1(1),h2(1)

      imesh = 1

c     IF (IFSTRS) THEN
c        MATMOD = 0
c        CALL AXHMSF (OUT1,OUT2,OUT3,INP1,INP2,INP3,H1,H2,MATMOD)
c     ELSE

c     Later - we can make this fast

      call axhelm_acc(out1,inp1,h1,h2,imesh,1)
      call axhelm_acc(out2,inp2,h1,h2,imesh,2)
      call axhelm_acc(out3,inp3,h1,h2,imesh,3)

      return
      end
c-----------------------------------------------------------------------
      subroutine wgradm1_acc(ux,uy,uz,u,nel) ! weak form of grad 
c
c     Compute gradient -- mesh 1 to mesh 1 (vel. to vel.)
c
      include 'SIZE'
      include 'DXYZ'
      include 'GEOM'
      include 'INPUT'
      include 'TSTEP'
      include 'WZ'

      parameter (lxyz=lx1*ly1*lz1)
      real ux(lxyz,1),uy(lxyz,1),uz(lxyz,1),u(lx1,ly1,lz1,1)

      real ur(lx1,ly1,lz1),us(lx1,ly1,lz1),ut(lx1,ly1,lz1)

      integer e,q

!$acc parallel loop present(rxm1,sxm1,txm1,dxm1
!$acc&                     ,rxm1,sxm1,txm1
!$acc&                     ,rxm1,sxm1,txm1) gang
!$acc&              private(ur,us,ut)
      do e=1,nel

!$acc    loop collapse(3) vector
         do k=1,lz1
         do j=1,ly1
         do i=1,lx1
            w1 = 0.0
            w2 = 0.0
            w3 = 0.0
!$acc       loop seq
            do q=1,lx1
               w1 = w1 + dxm1(i,q)*u(q,j,k,e) ! D u
               w2 = w2 + dxm1(j,q)*u(i,q,k,e) ! D u
               w3 = w3 + dxm1(k,q)*u(i,j,q,e) ! D u
            enddo
!$acc       end loop
            ur(i,j,k)=w1
            us(i,j,k)=w2
            ut(i,j,k)=w3
         enddo
         enddo
         enddo
!$acc    end loop

!$acc    loop vector
         do i=1,lxyz
            ux(i,e)=w3m1(i,1,1)*(ur(i,1,1)*rxm1(i,1,1,e)
     $                         + us(i,1,1)*sxm1(i,1,1,e)
     $                         + ut(i,1,1)*txm1(i,1,1,e) )
            uy(i,e)=w3m1(i,1,1)*(ur(i,1,1)*rym1(i,1,1,e)
     $                         + us(i,1,1)*sym1(i,1,1,e)
     $                         + ut(i,1,1)*tym1(i,1,1,e) )
            uz(i,e)=w3m1(i,1,1)*(ur(i,1,1)*rzm1(i,1,1,e)
     $                         + us(i,1,1)*szm1(i,1,1,e)
     $                         + ut(i,1,1)*tzm1(i,1,1,e) )
         enddo
      enddo

      return
      end
c-----------------------------------------------------------------------
      subroutine sumab_vx_acc ! Extrapolate velocity
c
c     sum up AB/BDF contributions 
c
      include 'SIZE'
      include 'TOTAL'

      n = lx1*ly1*lz1*nelv

!$acc enter data copyin (ab)

      write(6,*) 'this is nab:',nab,n
      if (nab.eq.3) then
!$acc    parallel loop
!$acc&   present(ab,vx,vy,vz,vxlag,vylag,vzlag,vx_e,vy_e,vz_e)
         do i=1,n
            vx_e(i)=ab(1)*vx(i,1,1,1)
     $             +ab(2)*vxlag(i,1,1,1,1)
     $             +ab(3)*vxlag(i,1,1,1,2)
            vy_e(i)=ab(1)*vy(i,1,1,1)
     $             +ab(2)*vylag(i,1,1,1,1)
     $             +ab(3)*vylag(i,1,1,1,2)
            vz_e(i)=ab(1)*vz(i,1,1,1)
     $             +ab(2)*vzlag(i,1,1,1,1)
     $             +ab(3)*vzlag(i,1,1,1,2)
         enddo
!$acc    end parallel
      else
      write(6,*) 'AB:',ab(1),ab(2)
!$acc    parallel loop
!$acc&   present(ab,vx,vy,vz,vxlag,vylag,vzlag,vx_e,vy_e,vz_e)
         do i=1,n
            vx_e(i)=ab(1)*vx(i,1,1,1)
     $             +ab(2)*vxlag(i,1,1,1,1)
            vy_e(i)=ab(1)*vy(i,1,1,1)
     $             +ab(2)*vylag(i,1,1,1,1)
            vz_e(i)=ab(1)*vz(i,1,1,1)
     $             +ab(2)*vzlag(i,1,1,1,1)
         enddo
!$acc    end parallel
      endif
!$acc exit data
      write(6,*) 'done nab:',nab,n

      return
      end
c-----------------------------------------------------------------------
      subroutine bcdirvc_acc(v1,v2,v3,m1,m2,m3) ! Dirichlet 
c
c     sum up AB/BDF contributions 
c
      include 'SIZE'
      include 'NEKUSE'
      parameter (lx=lx1*ly1*lz1)
      real v1(lx*nelv),v2(lx*nelv),v3(lx*nelv)
      real m1(lx*nelv),m2(lx*nelv),m3(lx*nelv)

      n = lx1*ly1*lz1*nelt

!$acc enter data copyin(ub,vb,wb)  !! Need to fix this!

!$acc parallel loop present(v1,v2,v3,m1,m2,m3,ub,vb,wb)
      do i=1,n
        v1(i)=m1(i)*v1(i)+(1-m1(i))*ub(i,1,1,1)
        v2(i)=m2(i)*v2(i)+(1-m2(i))*vb(i,1,1,1)
        v3(i)=m3(i)*v3(i)+(1-m3(i))*wb(i,1,1,1)
      enddo
!$acc end parallel

      return
      end
c-----------------------------------------------------------------------
      subroutine chck(s3)
      include 'SIZE'
      character*3 s3
      write(6,*) 'checker: ',s3
      return
      end
c-----------------------------------------------------------------------
      subroutine chk2(s3,a)
      include 'SIZE'
      character*3 s3
      parameter (lt=lx1*ly1*lz1*lelt)
      real a(lt)
      
      n=nx1*ny1*nz1*nelt
      amx=glamax_acc(a,n)
      write(6,*) 'check2: ',s3,amx

      return
      end
c-----------------------------------------------------------------------
      subroutine hmh_gmres_acc(res,h1,h2,wt,iter)

c     Solve the Helmholtz equation by right-preconditioned
c     GMRES iteration.

      include 'SIZE'
      include 'TOTAL'
      include 'FDMH1'
      include 'GMRES'
      common  /ctolpr/ divex
      common  /cprint/ ifprint
      logical          ifprint
      parameter (lt=lx1*ly1*lz1*lelv)
      real res(lt),h1(lt),h2(lt),wt(lt)

      real alpha, l, temp, temp_ptr1(1), temp_ptr2(1)
      integer outer

      logical if_hyb
      save    if_hyb
      data    if_hyb  /.false. /
      real    norm_fac
      save    norm_fac

      real*8 etime1,acctime1,dnekclock

      n = nx1*ny1*nz1*nelv

      etime1  = dnekclock()
      etime_p = 0.
      divex   = 0.
      iter    = 0
      m       = lgmres

      norm_fac = 1./sqrt(volvm1)

      acctime1 = dnekclock()

      tolps = abs(param(21))
      tolpss = tolps

      iconv = 0

      call chck('xgm')
      call rzero_acc(x_gmres,n)
      call chck('dxg')

      outer = 0
      do while (iconv.eq.0.and.iter.lt.500)
         outer = outer+1

         if(iter.eq.0) then
            call copy_acc  (r_gmres,res,n)               ! r = res
         else !update residual
            call ax        (w_gmres,x_gmres,h1,h2,n)     ! w = A x
            call sub3_acc  (r_gmres,res,w_gmres,n)       ! r = r - w
         endif



c        ROR: 2017-06-03: I inlined glsc3_acc because I couldn't figure
c        out how to call glsc3_acc from inside a kernel.  I kept getting
c        the error: "Unsupported nested compute construct in compute
c        construct or acc routine"
c        temp = sqrt(glsc3_acc(r_gmres,r_gmres,wt,n)) ! gamma  = \/ (r,r)

!$ACC DATA COPYOUT(temp_ptr1) CREATE(temp_ptr2)

!$ACC KERNELS PRESENT(r_gmres,wt)
         temp = 0.0
         do  k=1,n
           temp = temp + r_gmres(k)*r_gmres(k)*wt(k)
         enddo
         temp_ptr1(1) = temp
!$ACC END KERNELS

         call gop_acc(temp_ptr1,temp_ptr2,'+  ',1)

!$ACC KERNELS
         gamma_gmres(1) = sqrt(temp_ptr1(1))
!$ACC END KERNELS

!$ACC END DATA

         temp = sqrt(temp_ptr1(1))
         gamma_gmres(1) = sqrt(temp_ptr1(1))

         if(iter.eq.0) then
            div0 = temp*norm_fac
            if (param(21).lt.0) tolpss=abs(param(21))*div0
         endif

         !check for lucky convergence
         rnorm = 0.
         if(temp .eq. 0.) goto 9000
         call cmult2_acc(v_gmres(1,1),r_gmres,1./temp,n) ! v  = r / gamma
                                                   !  1            1
         do j=1,m
            iter = iter+1
            etime2 = dnekclock()
c . . . . . Overlapping Schwarz + coarse-grid . . . . . . .
            call h1mg_solve(z_gmres(1,j),v_gmres(1,j),if_hyb) ! z  = M  v
            call ortho_acc (z_gmres(1,j)) ! Orthogonalize wrt null space, if present
            etime_p = etime_p + dnekclock()-etime2

            call ax  (w_gmres,z_gmres(1,j),h1,h2,n) ! w = A z

            do i=1,j 
               temp = sqrt(glsc3_acc(w_gmres,v_gmres(k,i),wt,n))
               h_gmres(i,j) = temp
            enddo

            call gop_acc(h_gmres(1,j),wk_gmres,'+  ',j)

            do i=1,j
!$acc          parallel loop 
               do k=1,n
                  w_gmres(k) = w_gmres(k) - h_gmres(i,j) * v_gmres(k,i)
               enddo
!$acc          end loop
            enddo

            do i=1,j-1                  ! Apply Givens rotations to new column
               temp = h_gmres(i,j)
               h_gmres(i  ,j)=  c_gmres(i)*temp
     $                        + s_gmres(i)*h_gmres(i+1,j)
               h_gmres(i+1,j)= -s_gmres(i)*temp
     $                        + c_gmres(i)*h_gmres(i+1,j)
            enddo
                                                          !            ______
            alpha = sqrt(glsc3_acc(w_gmres,w_gmres,wt,n)) ! alpha =  \/ (w,w)
            rnorm = 0.
            if(alpha.eq.0.) goto 900  !converged

            l = sqrt(h_gmres(j,j)*h_gmres(j,j)+alpha*alpha)
            temp = 1./l
            c_gmres(j) = h_gmres(j,j) * temp
            s_gmres(j) = alpha  * temp
            h_gmres(j,j) = l
            gamma_gmres(j+1) = -s_gmres(j) * gamma_gmres(j)
            gamma_gmres(j)   =  c_gmres(j) * gamma_gmres(j)

            rnorm = abs(gamma_gmres(j+1))*norm_fac
            ratio = rnorm/div0

            if (ifprint.and.nio.eq.0)
     $         write (6,66) iter,tolpss,rnorm,div0,ratio,istep
   66       format(i5,1p4e12.5,i8,' Divergence')

            if (rnorm .lt. tolpss) goto 900  !converged
            if (j.eq.m) goto 1000 !not converged, restart

            temp = 1./alpha
            call cmult2_acc(v_gmres(1,j+1),w_gmres,temp,n) ! v    = w / alpha

         enddo

  900    iconv = 1
 1000    continue
         !back substitution
         !     -1
         !c = H   gamma

         do k=j,1,-1
            temp = gamma_gmres(k)
            do i=j,k+1,-1
               temp = temp - h_gmres(k,i)*c_gmres(i)
            enddo
            c_gmres(k) = temp/h_gmres(k,k)
         enddo

c        Sum of Arnoldi vectors
c
c        ROR: 2016-06-13: For OpenACC, we inlined the call to
c        add2s2() so the compiler could infer some nested
c        parallelism.
         do i=1,j
         do k=1,n
            x_gmres(k) = x_gmres(k) + z_gmres(k,i)*c_gmres(i)
         enddo
         enddo                                                !          i,j  i
      enddo
 9000 continue

      divex = rnorm

      call copy_acc(res,x_gmres,n)

      acctime1 = dnekclock()-acctime1


      etime1 = dnekclock()-etime1
      if (nio.eq.0) write(6,9999) istep,iter,divex,div0,tolpss,etime_p,
     &                            etime1,if_hyb
      if (nio.eq.0) write(6,*) 'acc_time: ', acctime1
 9999 format(4x,i7,'  PRES gmres ',4x,i5,1p5e13.4,1x,l4)


      return
      end
c-----------------------------------------------------------------------
      subroutine chk3(s3,a)
      include 'SIZE'
      character*3 s3
      parameter (lt=lx1*ly1*lz1*lelt)
      real a(lt)

      n=nx1*ny1*nz1*nelt
      amx=glamax(a,n)
      write(6,*) 'check2: ',s3,amx

      return
      end
c-----------------------------------------------------------------------
      subroutine opcolv3c_acc(a1,a2,a3,b1,b2,b3,c,d)
      include 'SIZE'
      REAL A1(LX1*LY1*LZ1*LELT),
     $     A2(LX1*LY1*LZ1*LELT),
     $     A3(LX1*LY1*LZ1*LELT),
     $     B1(LX1*LY1*LZ1*LELT),
     $     B2(LX1*LY1*LZ1*LELT),
     $     B3(LX1*LY1*LZ1*LELT),
     $     C (LX1*LY1*LZ1*LELT)

      NTOT1=LX1*LY1*LZ1*LELT

      IF (LDIM.EQ.3) THEN
!$ACC PARALLEL LOOP PRESENT(a1,a2,a3,b1,b2,b3,c)
         DO I=1,NTOT1
            A1(I)=B1(I)*C(I)*d
            A2(I)=B2(I)*C(I)*d
            A3(I)=B3(I)*C(I)*d
         END DO
!$ACC END PARALLEL
      ELSE
!$ACC PARALLEL LOOP PRESENT(a1,a2,b1,b2,c)
         DO I=1,NTOT1
            A1(I)=B1(I)*C(I)*d
            A2(I)=B2(I)*C(I)*d
         END DO
!$ACC END PARALLEL
      ENDIF
      return
      END
c-----------------------------------------------------------------------
      subroutine opadd2_acc (a1,a2,a3,b1,b2,b3)
      include 'SIZE'
      REAL A1(LX1*LY1*LZ1*LELV),
     $     A2(LX1*LY1*LZ1*LELV),
     $     A3(LX1*LY1*LZ1*LELV),
     $     B1(LX1*LY1*LZ1*LELV),
     $     B2(LX1*LY1*LZ1*LELV),
     $     B3(LX1*LY1*LZ1*LELV)
      NTOT1=LX1*LY1*LZ1*LELV
      CALL ADD2_ACC(A1,B1,NTOT1)
      CALL ADD2_ACC(A2,B2,NTOT1)
      IF(LDIM.EQ.3)CALL ADD2_ACC(A3,B3,NTOT1)
      return
      END
c-----------------------------------------------------------------------
      subroutine opadd2col_acc(a1,a2,a3,b1,b2,b3,c)
      include 'SIZE'
      REAL A1(LX1*LY1*LZ1*LELT),
     $     A2(LX1*LY1*LZ1*LELT),
     $     A3(LX1*LY1*LZ1*LELT),
     $     B1(LX1*LY1*LZ1*LELT),
     $     B2(LX1*LY1*LZ1*LELT),
     $     B3(LX1*LY1*LZ1*LELT),
     $     C(LX1*LY1*LZ1*LELT)

      NTOT1=LX1*LY1*LZ1*LELT

      IF (LDIM.EQ.3) THEN
!$ACC PARALLEL LOOP PRESENT(A1,A2,A3,B1,B2,B3,C)
         DO I=1,NTOT1
            A1(I)=A1(I)+b1(i)*c(i)
            A2(I)=A2(I)+b2(i)*c(i)
            A3(I)=A3(I)+b3(i)*c(i)
         END DO
!$ACC END PARALLEL
      ELSE
!$ACC PARALLEL LOOP PRESENT(A1,A2,B1,B2,C)
         DO I=1,NTOT1
            A1(I)=A1(I)+b1(i)*c(i)
            A2(I)=A2(I)+b2(i)*c(i)
         END DO
!$ACC END PARALLEL
      ENDIF
      return
      END
