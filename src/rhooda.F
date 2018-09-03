! 
! Copyright (C) 1996-2016	The SIESTA group
!  This file is distributed under the terms of the
!  GNU General Public License: see COPYING in the top directory
!  or http://www.gnu.org/copyleft/gpl.txt.
! See Docs/Contributors.txt for a list of contributors.
!
      subroutine rhooda( no, np, Datm, rhoatm, iaorb, iphorb, isa )
!! author: P.Ordejon and J.M.Soler
!! date: May 1995
!!
!! Finds the Harris density at the mesh points from the atomic occupations.
!!
!! Inverted so that grid points are the outer loop, J.D. Gale, Jan'99

! Modules
      use precision, only: dp, grid_p

      use atmfuncs, only: rcut, phiatm
      use atomlist, only: indxuo
      use mesh,     only: nsp, dxa, xdop, xdsp
      use meshphi

      implicit none

      integer, intent(in)       ::    no
        !! Number of basis orbitals
      integer, intent(in)       ::    np
        !! Number of mesh points
      integer, intent(in)       ::    iaorb(*)
        !! Pointer to atom to which orbital belongs
      integer, intent(in)       ::    iphorb(*)
        !! Orbital index within each atom
      integer, intent(in)       ::    isa(*)
        !! Species index of all atoms
      real(grid_p), intent(out) ::    rhoatm(nsp,np)
        !! Harris (sum of atoms) density at mesh points
      real(dp), intent(in)      ::    Datm(no)
        !! Occupations of basis orbitals in free atom

      real(dp)     :: phip

      integer      ::  i, ip, isp, iu, kn, iop, is, iphi, ia, ix
      real(dp)     ::  Ci, gradCi(3), r2o, r2sp, dxsp(3) 

#ifdef DEBUG
      call write_debug( '      Pre rhooda' )
#endif
!  Loop on mesh points
      do ip = 1,np

!  Initialise rhoatm
        do isp = 1, nsp
          rhoatm(isp,ip) = 0.0_grid_p
        enddo

!  Loop on orbitals of mesh point
        do kn = 1+endpht(ip-1), endpht(ip)
          i = lstpht(kn)
          iu = indxuo(i)

          if (DirectPhi) then
!  Generate phi value and loop on subpoints
            iphi = iphorb(i)
            ia = iaorb(i)
            is = isa(ia)
            r2o = rcut(is,iphi)**2
            iop = listp2(kn)
            do isp = 1,nsp
              do ix = 1,3
                dxsp(ix) = xdop(ix,iop) + xdsp(ix,isp) - dxa(ix,ia)
              enddo
              r2sp = dxsp(1)**2 + dxsp(2)**2 + dxsp(3)**2
              if (r2sp.lt.r2o) then
                call phiatm(is,iphi,dxsp,phip,gradCi)
                Ci = phip
                rhoatm(isp,ip) = rhoatm(isp,ip) + Datm(iu) * Ci * Ci
              endif
            enddo
          else
!  Loop on sub-points
            do isp = 1,nsp
              Ci = phi(isp,kn)
              rhoatm(isp,ip) = rhoatm(isp,ip) + Datm(iu) * Ci * Ci
            enddo
          endif

        enddo

      enddo

#ifdef DEBUG
      call write_debug( '      POS rhooda' )
#endif
      end subroutine rhooda
