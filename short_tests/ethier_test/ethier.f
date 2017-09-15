c  Exact fully 3D Navier-Stokes benchmark
c   
c  Reference:
c
c    C Ross Ethier, David Steinman,
c    Exact fully 3D Navier-Stokes solutions for benchmarking,
c    International Journal for Numerical Methods in Fluids,
c    Volume 19, Number 5, March 1994, pages 369-375.
c
c  Setup:
c
c    [-1,1] cube centered at (0,0,0), 
c    from t = 0 to t = 0.1, with parameters a = PI/4 and d = PI/2,
c    and with Dirichlet boundary conditions on all faces of the cube.
c
C-----------------------------------------------------------------------
      subroutine uservp(ix,iy,iz,eg) ! set variable properties

      include 'SIZE'
      include 'TOTAL'
      include 'NEKUSE'

      integer e,f,eg
c     e = gllel(eg)

      udiff  = 0.0
      utrans = 0.0

      return
      end
c-----------------------------------------------------------------------
      subroutine userf(ix,iy,iz,eg) ! set acceleration term
c
c     Note: this is an acceleration term, NOT a force!
c     Thus, ffx will subsequently be multiplied by rho(x,t).
c
      include 'SIZE'
      include 'TOTAL'
      include 'NEKUSE'

      integer e,f,eg
c     e = gllel(eg)

      ffx = 0.0
      ffy = 0.0
      ffz = 0.0

      return
      end
c-----------------------------------------------------------------------
      subroutine userq(ix,iy,iz,eg) ! set source term

      include 'SIZE'
      include 'TOTAL'
      include 'NEKUSE'

      integer e,f,eg
c     e = gllel(eg)

      qvol   = 0.0
      source = 0.0

      return
      end
c-----------------------------------------------------------------------
      subroutine userbc(ix,iy,iz,f,eg) ! set up boundary conditions

c     NOTE: This routine may or may not be called by every processor

      include 'SIZE'
      include 'TOTAL'
      include 'NEKUSE'

      integer e,f,eg

      call exactu(ux,uy,uz,x,y,z,1)

      return
      end
c-----------------------------------------------------------------------
      subroutine useric(ix,iy,iz,eg) ! set up initial conditions

      include 'SIZE'
      include 'TOTAL'
      include 'NEKUSE'
      integer e,eg

      call exactu(ux,uy,uz,x,y,z,1)

      return
      end
c-----------------------------------------------------------------------
      subroutine userchk

      include 'SIZE'
      include 'TOTAL'

      real uxe  (lx1,ly1,lz1,lelv)
      real uye  (lx1,ly1,lz1,lelv)
      real uze  (lx1,ly1,lz1,lelv)
      real uxerr(lx1,ly1,lz1,lelv)
      real uyerr(lx1,ly1,lz1,lelv)
      real uzerr(lx1,ly1,lz1,lelv)

      real pre  (lx2,ly2,lz2,lelv)
      real prerr(lx2,ly2,lz2,lelv)

      real hxyz(ldim,lhis)

      n  = nelv*nx1*ny1*nz1
      n2 = nelv*nx2*ny2*nz2

      call exactu(uxe,uye,uze,xm1,ym1,zm1,n)
      call exactp(pre,xm2,ym2,zm2,n2)

      call chsign(pre,n2)
      pbar  = glsc2(pr ,bm2,n2)/volvm2
      pbre  = glsc2(pre,bm2,n2)/volvm2
      pbarm = -pbre
      call cadd(pre,pbarm,n2)     !  Make sure pr and pre have same
      call cadd(pre,pbar,n2)      !  average pressure before comparing.

c     if (istep.le.5) then        !  Reset velocity & pressure to eliminate
c        call copy (vx,uxe,n)     !  start-up contributions
c        call copy (vy,uye,n)
c        call copy (vz,uze,n)
c        call copy (pr,pre,n2)
c     endif

      call sub3(uxerr,vx,uxe,n)
      call sub3(uyerr,vy,uye,n)
      call sub3(uzerr,vz,uze,n)
      uxerrl2 = glsc3(uxerr,bm1,uxerr,n)
      uxerrl2 = sqrt(uxerrl2)

      call sub3(prerr,pr,pre,n2)
      prerrl2 = glsc3(prerr,bm2,prerr,n2)
      prerrl2 = sqrt(prerrl2)

      if (mod(istep,iostep).eq.0) write(6,*) 'L2 err',istep,uxerrl2,
     $ prerrl2

      return
      end
c-----------------------------------------------------------------------
      subroutine usrdat()   ! This routine to modify element vertices

      include 'SIZE'
      include 'TOTAL'

      param(66) = 0
      param(67) = 0

      return
      end
c-----------------------------------------------------------------------
      subroutine usrdat2()  ! This routine to modify mesh coordinates

      include 'SIZE'
      include 'TOTAL'

      integer e,f

c     do iel=1,nelt
c     do ifc=1,2*ndim
c        idss = bc(5,ifc,iel,1)
c        if (idss.eq.1) cbc(ifc,iel,1)='v  '
c     enddo
c     enddo

      ! assuming the original domain is [0,1]x[0,1]x[0,1]
      do i=1,lx1*ly1*lz1*nelv
         xm1(i,1,1,1) = xm1(i,1,1,1) * 8.
         ym1(i,1,1,1) = ym1(i,1,1,1) * 8.
         zm1(i,1,1,1) = zm1(i,1,1,1) * 8.
      enddo

      return
      end
c-----------------------------------------------------------------------
      subroutine usrdat3()

      include 'SIZE'
      include 'TOTAL'

      call fix_geom

      return
      end

c-----------------------------------------------------------------------
      subroutine exactu(uxe,uye,uze,x,y,z,n)

      include 'SIZE'
      include 'TOTAL'

      real uxe(n),uye(n),uze(n)
      real x(n),y(n),z(n)

      real w1(lx1*ly1*lz1*lelt)

      a = pi/4.
      d = pi/2. 
      call uvwp_ethier(param(2),a,d,n,x,y,z,time,uxe,uye,uze,w1)

      return
      end
c-----------------------------------------------------------------------
      subroutine exactp(pre,x,y,z,n)

      include 'SIZE'
      include 'TOTAL'

      real pre(n)
      real x(n),y(n),z(n)

      real w1(lx1*ly1*lz1*lelt)

      a = pi/4.
      d = pi/2. 
      call uvwp_ethier(param(2),a,d,n,x,y,z,time,w1,w1,w1,pre)

      return
      end
c-----------------------------------------------------------------------
      subroutine uvwp_ethier (nu,a,d,n,x,y,z,t,u,v,w,p)

      implicit none

      integer n

      real nu
      real a
      real cxy(n)
      real cyz(n)
      real czx(n)
      real d
      real e2t
      real ex(n)
      real exy(n)
      real ey(n)
      real eyz(n)
      real ez(n)
      real ezx(n)
      integer i
      real p(n)
      real sxy(n)
      real syz(n)
      real szx(n)
      real t
      real u(n)
      real v(n)
      real w(n)
      real x(n)
      real y(n)
      real z(n)

      do i = 1, n

        ex(i) = exp ( a * x(i) )
        ey(i) = exp ( a * y(i) )
        ez(i) = exp ( a * z(i) )

        e2t = exp  ( - nu * d * d * t )

        exy(i) = exp ( a * ( x(i) + y(i) ) )
        eyz(i) = exp ( a * ( y(i) + z(i) ) )
        ezx(i) = exp ( a * ( z(i) + x(i) ) )

        sxy(i) = sin ( a * x(i) + d * y(i) )
        syz(i) = sin ( a * y(i) + d * z(i) )
        szx(i) = sin ( a * z(i) + d * x(i) )

        cxy(i) = cos ( a * x(i) + d * y(i) )
        cyz(i) = cos ( a * y(i) + d * z(i) )
        czx(i) = cos ( a * z(i) + d * x(i) )

        u(i) = - a * ( ex(i) * syz(i) + ez(i) * cxy(i) ) * e2t
        v(i) = - a * ( ey(i) * szx(i) + ex(i) * cyz(i) ) * e2t
        w(i) = - a * ( ez(i) * sxy(i) + ey(i) * czx(i) ) * e2t
        p(i) = 0.5D+00 * a * a * e2t * e2t * (
     &    + ex(i) * ex(i) + 2.0D+00 * sxy(i) * czx(i) * eyz(i)
     &    + ey(i) * ey(i) + 2.0D+00 * syz(i) * cxy(i) * ezx(i)
     &    + ez(i) * ez(i) + 2.0D+00 * szx(i) * cyz(i) * exy(i) )

      end do

      return
      end
c------------------------------------------------------------------------
      subroutine usrsetvert(glo_num,nel,nx,ny,nz) ! to modify glo_num

      integer*8 glo_num(1)

      return
      end
c------------------------------------------------------------------------
      subroutine userqtl

      call userqtl_scig

      return
      end
c------------------------------------------------------------------------
