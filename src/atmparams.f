! 
! Copyright (C) 1996-2016	The SIESTA group
!  This file is distributed under the terms of the
!  GNU General Public License: see COPYING in the top directory
!  or http://www.gnu.org/copyleft/gpl.txt.
! See Docs/Contributors.txt for a list of contributors.
!
      module atmparams
!! Hard-wired parameters (to be eliminated in the future)

      implicit none

      integer, parameter, public  :: nzetmx =  200
        !! Maximum number of PAOs or polarization orbitals
        !! with the same angular  momentum and
        !! for the same species.
      integer, parameter, public  :: nkbmx  =    4
        !! Maximum number of Kleinman-Bylander projectors
        !! for each angular momentum
        !!
        !! For the off-site SO calculation plus semicore states
        !! there will be at least 4 KBs for each l angular momentum
        !! (for each l shell we have `J = l +/- 1/2` )
      integer, parameter, public  :: nsmx  =    2
        !! Maximum number of semicore shells for each angular
        !! momentum present in the atom ( for normal atom `nsmx=0` )
      integer, parameter, public  :: nsemx = 1 + nsmx
      integer, parameter, public  :: ntbmax =  500
        !! Maximum number of points in the tables defining
        !! orbitals, projectors and local neutral-atom
        !! pseudopotential.
      integer, parameter, public  :: lmaxd  =    4
        !! Maximum angular momentum for both orbitals and projectors.
      integer, parameter, public  :: lmx2   = (lmaxd+1)*(lmaxd+1)
      integer, parameter, public  :: nrmax  = 20000
        !! Maximum number of points in the functions read
        !! from file '.vps' or '.psf' (this number is
        !! determined by the parameter nrmax in the
        !! program ATOM, which generates the files with
        !! the pseudopotential information). The number
        !! of points in the grid can be redefined if the
        !! pseudopotential is reparametrized.
        !! `nrmax = 20000` is a typical safe value in this case.
      integer, parameter, public  :: maxos=2*nzetmx*lmx2*nsemx

      private

      end module atmparams
