! ---
! Copyright (C) 1996-2016	The SIESTA group
!  This file is distributed under the terms of the
!  GNU General Public License: see COPYING in the top directory
!  or http://www.gnu.org/copyleft/gpl.txt .
! See Docs/Contributors.txt for a list of contributors.
! ---
      module mesh
!! Stores quantities that are connected with the mesh

      use precision, only : dp
      implicit none

      integer,  pointer, save :: idop(:)
        !! `idop(mop)`: Extended-mesh-index displacement of points
        !! within a sphere of radius `rmax`.
      integer,  pointer, save :: ipa(:) => null()
        !! `ipa(na)`: Mesh cell in which atom is
      real(dp), pointer, save :: dxa(:,:) => null()
        !! `dxa(3,na)`: Atom position within mesh-cell
      real(dp), pointer, save :: xdop(:,:)
        !! `xdop(3,mop)`: Vector to mesh points within rmax
      real(dp), pointer, save :: xdsp(:,:)
        !! `xdsp(3,nsp)`: Vector to mesh sub-points.
        !! Allocated in `[[dhscf_init(proc)]]`.
      integer,           save :: mop
        !! Maximum number of non-zero orbital points
      integer,           save :: ne(3)
        !! Number of mesh-Extension intervals in each direction
      integer,           save :: nem(3)
        !! Extended-mesh divisions in each direction
      integer,           save :: nmsc(3)
        !! Mesh divisions of each supercell vector
      integer,save :: nmuc(3)
        !! Mesh points in each unit cell vector
      integer,save :: nusc(3)
        !! Number of unit cells in each supercell dir
      integer,           save :: meshLim(2,3)
        !! Upper an lower limits of the mesh in every process
        !!
        !! My processor's box of mesh points:  
        !! `myBox(1,:)` : lower bounds  
        !! `myBox(2,:)` : upper bounds
      integer,           save :: nmeshg(3)
        !! Total number of mesh points in each direction
      integer,           save :: nsm
        !! Number of mesh sub-divisions in each direction
      integer,           save :: nsp
        !! Number of sub-points of each mesh point
      real(dp),          save :: cmesh(3,3)
        !! Mesh-cell vectors
      real(dp),          save :: rcmesh(3,3)
        !! Reciprocal mesh-cell vectors (WITHOUT \( 2*\pi \) factor)
      integer,  pointer, save :: indexp(:) => null()
        !! `indexp(nep)`: Translation from extended to normal mesh index
      integer,  pointer, save :: iatfold(:,:) => null()
        !! `iatfold(3,na)`: Supercell vector that keeps track of the
        !! of the folding of the atomic coordinates in the mesh

      end module mesh
