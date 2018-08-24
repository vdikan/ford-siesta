!
! Copyright (C) 1996-2016	The SIESTA group
!  This file is distributed under the terms of the
!  GNU General Public License: see COPYING in the top directory
!  or http://www.gnu.org/copyleft/gpl.txt.
! See Docs/Contributors.txt for a list of contributors.
!

#ifdef ASYNCHRONOUS_GRID_COMMS   /* Compile-time option */
# ifdef MPI
#   define ASYNCHRONOUS           /* Internal symbol */
# endif
#endif

!!  author: Rogeli Grima (BSC)
!!  date: December 2007
!!  license: GNU GPL
!!
!!  This file contains module moreMeshSubs. It defines and handles
!!  different parallel mesh distributions.
!!
!!  Includes the following subroutines connected to the mesh :
!!
!!  * `[[initMeshDistr(proc)]]` - Precompute a new data distribution for the
!!    grid mesh to be used in Hamiltonian construction.
!!  * `[[setMeshDistr(proc)]]`- Select a mesh distribution and set the grid sizes.
!!  * `[[distMeshData(proc)]]`- Move data from one data distribution to another.
!!  * `[[allocASynBuffer(proc)]]` - Allocate buffer for asynchronous
!!    communications  if necessary.

      MODULE moreMeshSubs
      !! author: Rogeli Grima (BSC)
      !! date: December 2007

      use precision, only : grid_p, dp, i8b
      use parallel,  only : node, Nodes, ProcessorY
      use sys,       only : die
      use alloc,     only : re_alloc, de_alloc

      implicit none

      PUBLIC :: initMeshDistr, setMeshDistr, allocExtMeshDistr
      PUBLIC :: allocIpaDistr, distMeshData, resetMeshDistr
#ifdef MPI
      PUBLIC :: initMeshExtencil, distExtMeshData, gathExtMeshData
#endif
      PUBLIC :: allocASynBuffer
!     Symbolic names for parallel mesh distributions

      integer, parameter, public :: UNIFORM = 1
      integer, parameter, public :: QUADRATIC = 2
      integer, parameter, public :: LINEAR = 3

!
!     Symbolic names for "reord"-type operations
!
      integer, parameter, public :: TO_SEQUENTIAL = +1
!! alias to translation direction for `[[reord(proc)]]` procedure
      integer, parameter, public :: TO_CLUSTER = -1
!! alias to translation direction for `[[reord(proc)]]` procedure
      integer, parameter, public :: KEEP       =  0

      PRIVATE

      interface distMeshData
!! Move data from vector `fsrc`, that uses distribution `iDistr`, to vector
!! `fdst`, that uses distribution `oDistr`. It also re-orders a clustered
!! data array into a sequential one and viceversa.
!! If this is a sequencial execution, it only reorders the data.

!!@note
!! There are two subroutines: one to deal with real data and
!! the other with integers. Both are called using the same interface.
!!@endnote

!!@note
!! *AG*: Note that the integer version does NOT have the exact functionality
!! of the real version. In particular, the integer version has no provision
!! for a "serial fallback", and so this case has been trapped.
!!@endnote

!! Check the communications that this process should do to move data
!! from `iDistr` to `oDistr`. We have 3 kind of communications (send, receive
!! and keep on the same node). We have 3 kind of reorderings (clustered to
!! sequential, sequential to clustered and keep the same ordering).

!! For the sequencial code we call subroutine `[[reord(proc)]]`

!!#### INPUT
!!
!! * `iDistr` : Distribution index of the input vector.
!! * `fsrc`   : Input vector.
!! * `oDistr` : Distribution index of the output vector.
!! * `itr`    : TRanslation-direction switch:  
!!     `itr`=+1 => From clustered to sequential  
!!     `itr`=-1 => From sequential to clustered  
!!     `itr`=0  => Keep the status
!!
!!
!!#### OUTPUT
!!
!! * `fdst`   : Output vector.
!
        module procedure distMeshData_rea, distMeshData_int
      end interface distMeshData

      TYPE meshDisType
        !! Private type to hold mesh distribution data.
      integer           :: nMesh(3)
        !! Number of mesh div. in each axis.
      integer,  pointer :: box(:,:,:)
        !! Mesh box bounds of each node:  
        !! box(1,iAxis,iNode)=lower bounds  
        !! box(2,iAxis,iNode)=upper bounds
      integer,  pointer :: indexp(:)
      integer,  pointer :: idop(:)
      real(dp), pointer :: xdop(:,:)
      integer,  pointer :: ipa(:)
      END TYPE meshDisType

      TYPE meshCommType
        !! Private type to hold communications to move data from one
        !! distribution to another.
      integer          :: ncom
        !! Number of needed communications
      integer, pointer :: src(:)
        !! Sources of communications
      integer, pointer :: dst(:)
        !! Destination of communications
      END TYPE meshCommType

      character(len=*),      parameter :: moduName = 'moreMeshSubs'
        !! Name of the module.
      integer,               parameter :: maxDistr = 5
        !! Maximum number of data distribution that can be handled
      integer,               parameter :: gp = grid_p
        !! Alias of the grid precision
      type(meshDisType),  target, save :: meshDistr(maxDistr)
        !! Contains information of the several data distributions
      type(meshCommType), target, save ::
     &                   meshCommu((maxDistr*(maxDistr-1))/2)
        !! Contains all the communications to move among the
        !! several data distributions
      type(meshCommType), target, save :: exteCommu(maxDistr,3)
        !! Contains all the needed communications to compute
        !! the extencil

#ifdef ASYNCHRONOUS
      real(grid_p),            pointer :: tBuff1(:)
        !! Memory buffer for asynchronous communications
      real(grid_p),            pointer :: tBuff2(:)
        !! Memory buffer for asynchronous communications
#endif

      CONTAINS

      subroutine initMeshDistr( iDistr, oDistr, nm, wload )
!! Computes a new data distribution and the communications needed to
!! move data from/to the current distribution to the existing ones.
!!
!! The limits of the new distributions are stored in the current module
!! in `[[moreMeshSubs(module):meshDistr(variable)]]`:
!!
!!     meshDistr(oDistr)
!!     meshCommu(((oDistr-2)*(oDistr-1))/2+1:(oDistr-1)*oDistr/2)
!!
!! If this is the first distribution, we split the mesh uniformly among
!! the several processes (we only split it in dimensions Y and Z).
!!
!! For the other data distributions we should split the vector wload.
!! The subroutine splitwload will return the limits of the new data
!! distribution. The subroutine compMeshComm will return the communications
!! needed to move data from/to the current distribution to/from the
!! previous ones.

      implicit none
      integer, optional,  intent(in) :: iDistr
        !!  Distribution index of the input vector
      integer,            intent(in) :: oDistr
        !!  The new data distribution index
      integer,            intent(in) :: nm(3)
        !!  Number of Mesh divisions of each cell vector
      integer, optional,  intent(in) :: wload(*)
        !!  Weights of every point of the mesh using the input distribution

!     Local variables
      character(len=*), parameter :: myName = moduName//'initMeshDistr '
      character(len=*), parameter :: errMsg = myName//'ERROR: '
      integer                     :: ii, jj, PY, PZ, PP, ProcessorZ,
     &                               blocY, blocZ, nremY, nremZ,
     &                               iniY, iniZ, dimY, dimZ, nsize
      type(meshDisType),  pointer :: distr
      logical,               save :: firstime=.true.
      integer, pointer            :: box(:,:,:), mybox(:,:)

      call timer( 'INITMESH', 1 )

!     Check the number of mesh distribution
      if (oDistr.gt.maxDistr)
     &  call die( errMsg // 'oDistr.gt.maxDistr' )

!     Reset data if necessay
      if (firstime) then
        do ii= 1, maxDistr
          nullify(meshDistr(ii)%box)
          nullify(meshDistr(ii)%indexp)
          nullify(meshDistr(ii)%idop)
          nullify(meshDistr(ii)%xdop)
          nullify(meshDistr(ii)%ipa)
        enddo
        do ii= 1, (maxDistr*(maxDistr-1))/2
          nullify(meshCommu(ii)%src)
          nullify(meshCommu(ii)%dst)
        enddo
        do ii= 1, maxDistr
          do jj= 1, 3
            nullify(exteCommu(ii,jj)%src)
            nullify(exteCommu(ii,jj)%dst)
          enddo
        enddo
#ifdef ASYNCHRONOUS
        nullify(tBuff1)
        nullify(tBuff2)
#endif
        firstime = .false.
      endif

      distr => meshDistr(oDistr)
!     Allocate memory for the current distribution
      nullify( distr%box )
      call re_alloc( distr%box, 1, 2, 1, 3, 1, Nodes,
     &               'distr%box', moduName )

!     The first distribution should be the uniform distribution
      if (oDistr.eq.1) then
        ProcessorZ = Nodes/ProcessorY
        blocY = (nm(2)/ProcessorY)
        blocZ = (nm(3)/ProcessorZ)
        nremY = nm(2) - blocY*ProcessorY
        nremZ = nm(3) - blocZ*ProcessorZ

        PP   = 1
        iniY = 1
        do PY = 1, ProcessorY
          dimY = blocY
          if (PY.LE.nremY) dimY = dimY + 1
          iniZ = 1
          do PZ = 1, ProcessorZ
            dimZ = blocZ
            if (PZ.LE.nremZ) dimZ = dimZ + 1

            distr%box(1,1,PP) = 1
            distr%box(2,1,PP) = nm(1)
            distr%box(1,2,PP) = iniY
            distr%box(2,2,PP) = iniY + dimY - 1
            distr%box(1,3,PP) = iniZ
            distr%box(2,3,PP) = iniZ + dimZ - 1

            iniZ = iniZ + dimZ
            PP   = PP + 1
          enddo
          iniY = iniY + dimY
        enddo
      else
!       In order to compute the other data distributions, we should split
!       the vector "wload" among the several processes
#ifdef MPI
        if (.NOT. present(iDistr) .OR.
     &      .NOT. present(wload) ) then
          call die( errMsg // 'Wrong parameters' )
        endif

        call splitwload( Nodes, node+1, nm, wload,
     &                   meshDistr(iDistr), meshDistr(oDistr) )

        call reordMeshNumbering( meshDistr(1), distr )

!       Precompute the communications needed to move data between the new data
!       distribution and the previous ones.
        jj = ((oDistr-2)*(oDistr-1))/2 + 1
        do ii=1, oDistr-1
          call compMeshComm( meshDistr(ii), distr, meshCommu(jj) )
          jj = jj + 1
        enddo
#endif
      endif
      if (Node == 0) then
         write(6,"(a,i3)")"New grid distribution: ", oDistr
         do PP= 1, Nodes
            write(6,"(i12,3x,3(i5,a1,i5))")
     $       PP,
     $       (distr%box(1,jj,PP), ":", distr%box(2,jj,PP), jj=1,3)
         enddo
      endif

      call timer( 'INITMESH', 2 )

      end subroutine initMeshDistr


      subroutine allocASynBuffer( ndistr )
!! Allocate memory buffers for asynchronous communications.
!! It does nothing for synchronous communications.  
!! The output values are stored in the current module:  
!! `[[moreMeshSubs(module):tBuff1(variable)]]` : Buffer for distribution 1  
!! `[[moreMeshSubs(module):tBuff2(variable)]]` : Buffer for other distributions
      use mesh,    only : nsm
      implicit none

      integer                     :: ndistr
        !! Total number of distributions

      integer          :: ii, jj, imax1, imax2, lsize, nsp, Lbox(2,3)
      integer, pointer :: box1(:,:), box2(:,:), nsize(:)
      logical          :: inters

#ifdef ASYNCHRONOUS
!     Allocate local memory
      nsp  = nsm*nsm*nsm
      call re_alloc( nsize, 1, ndistr, 'nsize', moduName )

!     Check the size of the local box for every data distribution
      do ii= 1, ndistr
        box1 => meshDistr(ii)%box(:,:,node+1)
        nsize(ii) = (box1(2,1)-box1(1,1)+1)*
     &              (box1(2,2)-box1(1,2)+1)*
     &              (box1(2,3)-box1(1,3)+1)*nsp
      enddo

!     Check the size of the intersections between the first data distributions
!     and the others data distributions.
!     Buffers don't need to store intersections
      imax1 = 0
      imax2 = 0
      box1 => meshDistr(1)%box(:,:,node+1)
      do ii= 2, ndistr
        box2 => meshDistr(ii)%box(:,:,node+1)
        call boxIntersection( box1, box2, Lbox, inters )
        if (inters) then
          lsize = (Lbox(2,1)-Lbox(1,1)+1)*
     &            (Lbox(2,2)-Lbox(1,2)+1)*
     &            (Lbox(2,3)-Lbox(1,3)+1)*nsp
        else
          lsize = 0
        endif
        imax1 = max(imax1,nsize(1)-lsize)
        imax2 = max(imax2,nsize(ii)-lsize)
      enddo

!     Deallocate local memory
      call de_alloc( nsize, 'nsize', moduName )

!     Allocate memory for asynchronous communications
      call re_alloc( tBuff1, 1, imax1, 'tBuff1', moduName )
      call re_alloc( tBuff2, 1, imax2, 'tBuff2', moduName )
#endif

      end subroutine allocASynBuffer


      subroutine allocExtMeshDistr( iDistr, nep, mop )
      use mesh, only: indexp, idop, xdop
      implicit none
!     Input variables
      integer,         intent(in) :: iDistr, nep, mop
!     Local variables
      type(meshDisType),  pointer :: distr

      distr => meshDistr(iDistr)
      call re_alloc( distr%indexp, 1, nep, 'distr%indexp', moduName )
      call re_alloc( distr%idop, 1, mop, 'distr%idop', moduName )
      call re_alloc( distr%xdop, 1, 3, 1, mop, 'distr%xdop', moduName )

      indexp => distr%indexp
      idop   => distr%idop
      xdop   => distr%xdop
      end subroutine allocExtMeshDistr


      subroutine allocIpaDistr( iDistr, na )
      use mesh, only: ipa
      implicit none
!     Input variables
      integer,         intent(in) :: iDistr, na
!     Local variables
      type(meshDisType),  pointer :: distr

      distr => meshDistr(iDistr)
      call re_alloc( distr%ipa, 1, na, 'distr%ipa', moduName )
      ipa => meshDistr(iDistr)%ipa

      end subroutine allocIpaDistr


      subroutine setMeshDistr( iDistr, nsm, nsp, nml, nmpl, ntml, ntpl )
!! Fixes the new data limits and dimensions of the mesh to those of
!! the data distribution `iDistr`.
      use mesh, only: meshLim, indexp, ipa, idop, xdop
      implicit none

      integer,  intent(in) :: iDistr
        !! Distribution index of the input vector
      integer,  intent(in) :: nsm
        !! Number of mesh sub-divisions in each direction
      integer,  intent(in) :: nsp
        !! Number of sub-points of each mesh point
      integer, intent(out) :: nml(3)
        !! Local number of Mesh divisions in each cell vector
      integer, intent(out) :: nmpl
        !! Local number of Mesh divisions
      integer, intent(out) :: ntml(3)
        !! Local number of Mesh points in each cell vector
      integer, intent(out) :: ntpl
        !! Local number of Mesh points

!     Local variables
      type(meshDisType),  pointer :: distr

      distr => meshDistr(iDistr)

      meshLim = distr%box(1:2,1:3,node+1)
      nml(1) = (MeshLim(2,1)-MeshLim(1,1)) + 1
      nml(2) = (MeshLim(2,2)-MeshLim(1,2)) + 1
      nml(3) = (MeshLim(2,3)-MeshLim(1,3)) + 1
      nmpl   = nml(1)*nml(2)*nml(3)
      ntml   = nml*nsm
      ntpl   = nmpl*nsp

      indexp => distr%indexp
      idop   => distr%idop
      xdop   => distr%xdop
      ipa    => distr%ipa
!--------------------------------------------------------------------------- END
      end subroutine setMeshDistr

      subroutine resetMeshDistr( iDistr )
!! Reset the data of the distribution `iDistr`.
!! Deallocate associated arrays of the current distribution.  
!! Modifies data of the current module.

      implicit none

      integer, optional,  intent(in) :: iDistr
        !! Distribution index to be reset

      integer                        :: idis, ini, fin, icom
      type(meshDisType),  pointer    :: distr
      type(meshCommType), pointer    :: mcomm

      if (present(iDistr)) then
        ini = iDistr
        fin = iDistr
      else
        ini = 1
        fin = maxDistr
      endif

      do idis= ini, fin
        distr => meshDistr(idis)

        distr%nMesh = 0

        if (associated(distr%box)) then
          call de_alloc( distr%box, 'distr%box', 'moreMeshSubs' )
        endif

        if (associated(distr%indexp)) then
          call de_alloc( distr%indexp, 'distr%indexp',
     &                   'moreMeshSubs' )
        endif

        if (associated(distr%idop)) then
          call de_alloc( distr%idop, 'distr%idop',
     &                   'moreMeshSubs' )
        endif

        if (associated(distr%xdop)) then
          call de_alloc( distr%xdop, 'distr%xdop',
     &                   'moreMeshSubs' )
        endif

        if (associated(distr%ipa)) then
          call de_alloc( distr%ipa, 'distr%ipa',
     &                   'moreMeshSubs' )
        endif

        do icom=1, 3
          mcomm => exteCommu(idis,icom)
          if (associated(mcomm%src)) then
            call de_alloc( mcomm%src, 'mcomm%src', 'moreMeshSubs' )
          endif
          if (associated(mcomm%dst)) then
            call de_alloc( mcomm%dst, 'mcomm%dst', 'moreMeshSubs' )
          endif
          mcomm%ncom = 0
        enddo

        do icom= ((idis-2)*(idis-1))/2 + 1, ((idis-1)*idis)/2
          mcomm => meshCommu(icom)
          if (associated(mcomm%src)) then
            call de_alloc( mcomm%src, 'mcomm%src', 'moreMeshSubs' )
          endif
          if (associated(mcomm%dst)) then
            call de_alloc( mcomm%dst, 'mcomm%dst', 'moreMeshSubs' )
          endif
          mcomm%ncom = 0
        enddo
      enddo

#ifdef ASYNCHRONOUS
      if (associated(tBuff1)) then
        call de_alloc(tBuff1, 'tBuff1', 'moreMeshSubs' )
      endif
      if (associated(tBuff2)) then
        call de_alloc(tBuff2, 'tBuff2', 'moreMeshSubs' )
      endif
#endif

      end subroutine resetMeshDistr


#ifdef ASYNCHRONOUS
      subroutine distMeshData_rea( iDistr, fsrc, oDistr, fdst, itr )
      use mesh,    only : nsm, nmeshg
#ifdef MPI
      use mpi_siesta
#endif
      implicit none
!     Passed arguments
      integer,         intent(in) :: iDistr, oDistr, itr
      real(grid_p),    intent(in) :: fsrc(*)
      real(grid_p),   intent(out) :: fdst(*)
!     Local variables
      integer                     :: i, I1, I2, I3, N1, N2, N3, NN, ind,
     &                               J1, J2, J3, K1, K2, K3, KS, KR,
     &                               icom, ncom, nsp, me, nsize, lsize,
     &                               NSRC(3), NDST(3), Lbox(2,3), ierr,
     &                               nm(3), status(MPI_Status_Size),
     &                               Xsize, Ysize, Zsize
      logical                     :: inters
      integer,            pointer :: request(:), src(:), dst(:),
     &                               Sbox(:,:), Dbox(:,:), JS(:)
      real(grid_p),       pointer :: sBuff(:), rBuff(:)
      type(meshDisType),  pointer :: idis, odis

#ifdef DEBUG
      call write_debug( '    PRE distMeshData' )
#endif
#ifdef _TRACE_
      call MPI_Barrier( MPI_Comm_World, ierr )
      call MPItrace_event( 1000, 6 )
#endif
      call timer( 'COMM_BSC', 1 )

      nm(1:3) = nmeshg(1:3) / nsm
      if (nodes == 1) then

        if (itr.gt.0) then
          ! Note that in reord the first argument is always
          ! clustered
          call reord( fsrc, fdst, nm, nsm, TO_SEQUENTIAL )
        else if (itr .lt. 0) then
          call reord( fdst, fsrc, nm, nsm, TO_CLUSTER )
        else
          ! Copy source to destination
          ! This will be executed only in serial mode,
          ! so we know that the size is the total number
          ! of (small) points, but maybe this information
          ! should be more explicit.
          nsize = product(nmeshg(1:3))
          fdst(1:nsize) = fsrc(1:nsize)
        endif
      else  ! nodes > 1
!       The communications are stored in a triangular structure.
        if (iDistr.gt.oDistr) then
          ind  = ((iDistr-1)*(iDistr-2))/2 + oDistr
          ncom = meshCommu(ind)%ncom
          src => meshCommu(ind)%dst
          dst => meshCommu(ind)%src
        else
          ind = ((oDistr-1)*(oDistr-2))/2 + iDistr
          ncom = meshCommu(ind)%ncom
          src => meshCommu(ind)%src
          dst => meshCommu(ind)%dst
        endif

        nullify( request )
        call re_alloc( request, 1, ncom, 'request', 'distmeshdata' )

        idis => meshDistr(iDistr)
        odis => meshDistr(oDistr)

        nsp = nsm*nsm*nsm
        me  = node + 1
        nullify( JS )
        call re_alloc( JS, 1, nsp, 'JS', 'distmeshdata' )

        if (iDistr.eq.UNIFORM) then
          sBuff => tBuff1(:)
          rBuff => tBuff2(:)
        else
          if (oDistr.eq.UNIFORM) then
            sBuff => tBuff2(:)
            rBuff => tBuff1(:)
          else
!           Asynchronous buffers are sized to move data from/to
!           UNIFORM distribution. Check subroutine allocASynBuffer
!           to contemplate different cases
            call die( 'Asynchronous temporal buffer error' )
          endif
        endif

        Sbox => idis%box(:,:,ME)
        NSRC(1) = (Sbox(2,1) - Sbox(1,1) + 1)*nsm
        NSRC(2) = (Sbox(2,2) - Sbox(1,2) + 1)*nsm
        NSRC(3) = (Sbox(2,3) - Sbox(1,3) + 1)*nsm

        Dbox => odis%box(:,:,ME)
        NDST(1) = (Dbox(2,1) - Dbox(1,1) + 1)*nsm
        NDST(2) = (Dbox(2,2) - Dbox(1,2) + 1)*nsm
        NDST(3) = (Dbox(2,3) - Dbox(1,3) + 1)*nsm

        if (itr.eq.1) then
!         From clustered to sequential
          NN = 1
          I3 = 0
          DO N3=0, NSM-1
            I2 = 0
            DO N2= 0, NSM-1
              I1 = I2 + I3
              DO N1= 0, NSM-1
                JS(NN) = I1
                NN     = NN + 1
                I1     = I1 + 1
              ENDDO
              I2 = I2 + NDST(1)
            ENDDO
            I3 = I3 + NDST(1)*NDST(2)
          ENDDO

          KS = 1
          KR = 1
          do icom= 1, ncom
            Sbox => idis%box(:,:,src(icom))
            Dbox => odis%box(:,:,dst(icom))
            call boxIntersection( Sbox, Dbox, Lbox, inters )

            if (src(icom).eq.ME) then
              if (dst(icom).eq.ME) then
!               SRC and DST are the current process
                J3 = (Lbox(1,3) - Sbox(1,3))*NSRC(1)*NSRC(2)*NSM
                K3 = (Lbox(1,3) - Dbox(1,3))*NDST(1)*NDST(2)*NSM
                do I3 = Lbox(1,3), Lbox(2,3)
                  J2 = (Lbox(1,2) - Sbox(1,2))*NSRC(1)*NSM*NSM
                  K2 = (Lbox(1,2) - Dbox(1,2))*NDST(1)*NSM
                  do I2 = Lbox(1,2), Lbox(2,2)
                    J1 = (Lbox(1,1) - Sbox(1,1))*NSP + 1 + J2 + J3
                    K1 = (Lbox(1,1) - Dbox(1,1))*NSM + 1 + K2 + K3
                    do I1 = Lbox(1,1), Lbox(2,1)
                      DO NN= 1, NSP
                        fdst(K1+JS(NN)) = fsrc(J1)
                        J1 = J1 + 1
                      ENDDO
                      K1 = K1 + NSM
                    enddo
                    J2 = J2 + NSRC(1)*NSM*NSM
                    K2 = K2 + NDST(1)*NSM
                  enddo
                  J3 = J3 + NSRC(1)*NSRC(2)*NSM
                  K3 = K3 + NDST(1)*NDST(2)*NSM
                enddo
              else
!               We should send data to process dst(icom)-1
                lsize = (Lbox(2,1) - Lbox(1,1) + 1)*
     &                  (Lbox(2,2) - Lbox(1,2) + 1)*
     &                  (Lbox(2,3) - Lbox(1,3) + 1)*nsp

                J3 = (Lbox(1,3) - Sbox(1,3))*NSRC(1)*NSRC(2)*NSM
                K1 = KS
                do I3 = Lbox(1,3), Lbox(2,3)
                  J2 = (Lbox(1,2) - Sbox(1,2))*NSRC(1)*NSM*NSM
                  do I2 = Lbox(1,2), Lbox(2,2)
                    J1 = (Lbox(1,1) - Sbox(1,1))*NSP + 1 + J2 + J3
                    do I1 = Lbox(1,1), Lbox(2,1)
                      DO NN= 1, NSP
                        sBuff(K1) = fsrc(J1)
                        J1 = J1 + 1
                        K1 = K1 + 1
                      ENDDO
                    enddo
                    J2 = J2 + NSRC(1)*NSM*NSM
                  enddo
                  J3 = J3 + NSRC(1)*NSRC(2)*NSM
                enddo
#ifdef MPI
                call mpi_isend( sBuff(KS), lsize, MPI_grid_real,
     &                          dst(icom)-1, 0, MPI_COMM_WORLD,
     &                          request(icom), ierr )
#endif
                KS = K1
              endif
            else
!             We should receive data from process src(icom)-1
              lsize = (Lbox(2,1) - Lbox(1,1) + 1)*
     &                (Lbox(2,2) - Lbox(1,2) + 1)*
     &                (Lbox(2,3) - Lbox(1,3) + 1)*nsp
#ifdef MPI
              call mpi_irecv( rBuff(KR), lsize, MPI_grid_real,
     &                        src(icom)-1, 0, MPI_COMM_WORLD,
     &                        request(icom), ierr )
#endif
              KR = KR + lsize
            endif
          enddo

          J1 = 1
          do icom= 1, ncom
!           Wait for received data and move it to the destination buffer
            if (src(icom).ne.ME) then
              Sbox => idis%box(:,:,src(icom))
              Dbox => odis%box(:,:,dst(icom))
              call boxIntersection( Sbox, Dbox, Lbox, inters )
#ifdef MPI
              CALL MPI_WAIT( request(icom), status, ierr )
#endif
              K3 = (Lbox(1,3) - Dbox(1,3))*NDST(1)*NDST(2)*NSM
              do I3 = Lbox(1,3), Lbox(2,3)
                K2 = (Lbox(1,2) - Dbox(1,2))*NDST(1)*NSM
                do I2 = Lbox(1,2), Lbox(2,2)
                  K1 = (Lbox(1,1) - Dbox(1,1))*NSM + 1 + K2 + K3
                  do I1 = Lbox(1,1), Lbox(2,1)
                    DO NN= 1, NSP
                      fdst(K1+JS(NN)) = rBuff(J1)
                      J1 = J1 + 1
                    ENDDO
                    K1 = K1 + NSM
                  enddo
                  K2 = K2 + NDST(1)*NSM
                enddo
                K3 = K3 + NDST(1)*NDST(2)*NSM
              enddo
            endif
          enddo
        else if (itr.eq.-1) then
!         From sequencial to clustered
          NN = 1
          I3 = 0
          DO N3=0, NSM-1
            I2 = 0
            DO N2= 0, NSM-1
              I1 = I2 + I3
              DO N1= 0, NSM-1
                JS(NN) = I1
                NN     = NN + 1
                I1     = I1 + 1
              ENDDO
              I2 = I2 + NSRC(1)
            ENDDO
            I3 = I3 + NSRC(1)*NSRC(2)
          ENDDO

          KS = 1
          KR = 1
          do icom= 1, ncom
            Sbox => idis%box(:,:,src(icom))
            Dbox => odis%box(:,:,dst(icom))
            call boxIntersection( Sbox, Dbox, Lbox, inters )

            if (src(icom).eq.ME) then
              if (dst(icom).eq.ME) then
!               SRC and DST are the current process
                J3 = (Lbox(1,3) - Sbox(1,3))*NSRC(1)*NSRC(2)*NSM
                K3 = (Lbox(1,3) - Dbox(1,3))*NDST(1)*NDST(2)*NSM
                do I3 = Lbox(1,3), Lbox(2,3)
                  J2 = (Lbox(1,2) - Sbox(1,2))*NSRC(1)*NSM
                  K2 = (Lbox(1,2) - Dbox(1,2))*NDST(1)*NSM*NSM
                  do I2 = Lbox(1,2), Lbox(2,2)
                    J1 = (Lbox(1,1) - Sbox(1,1))*NSM + 1 + J2 + J3
                    K1 = (Lbox(1,1) - Dbox(1,1))*NSP + 1 + K2 + K3
                    do I1 = Lbox(1,1), Lbox(2,1)
                      DO NN= 1, NSP
                        fdst(K1) = fsrc(J1+JS(NN))
                        K1 = K1 + 1
                      ENDDO
                      J1 = J1 + NSM
                    enddo
                    J2 = J2 + NSRC(1)*NSM
                    K2 = K2 + NDST(1)*NSM*NSM
                  enddo
                  J3 = J3 + NSRC(1)*NSRC(2)*NSM
                  K3 = K3 + NDST(1)*NDST(2)*NSM
                enddo
              else
!               We should send data to process dst(icom)-1
                lsize = (Lbox(2,1) - Lbox(1,1) + 1)*
     &                  (Lbox(2,2) - Lbox(1,2) + 1)*
     &                  (Lbox(2,3) - Lbox(1,3) + 1)*nsp

                J3 = (Lbox(1,3) - Sbox(1,3))*NSRC(1)*NSRC(2)*NSM
                K1 = KS
                do I3 = Lbox(1,3), Lbox(2,3)
                  J2 = (Lbox(1,2) - Sbox(1,2))*NSRC(1)*NSM
                  do I2 = Lbox(1,2), Lbox(2,2)
                    J1 = (Lbox(1,1) - Sbox(1,1))*NSM + 1 + J2 + J3
                    do I1 = Lbox(1,1), Lbox(2,1)
                      DO NN= 1, NSP
                        sBuff(K1) = fsrc(J1+JS(NN))
                        K1 = K1 + 1
                      ENDDO
                      J1 = J1 + NSM
                    enddo
                    J2 = J2 + NSRC(1)*NSM
                  enddo
                  J3 = J3 + NSRC(1)*NSRC(2)*NSM
                enddo
#ifdef MPI
                call mpi_isend( sBuff(KS), lsize, MPI_grid_real,
     &                          dst(icom)-1, 0, MPI_COMM_WORLD,
     &                          request(icom), ierr )
#endif
                KS = K1
              endif
            else
C             We should receive data from process src(icom)-1
              lsize = (Lbox(2,1) - Lbox(1,1) + 1)*
     &                (Lbox(2,2) - Lbox(1,2) + 1)*
     &                (Lbox(2,3) - Lbox(1,3) + 1)*nsp
#ifdef MPI
              call mpi_irecv( rBuff(KR), lsize, MPI_grid_real,
     &                        src(icom)-1, 0, MPI_COMM_WORLD,
     &                        request(icom), ierr )
#endif
              KR = KR + lsize
            endif
          enddo

          J1 = 1
          do icom= 1, ncom
!           Wait for received data and move it to the destination buffer
            if (src(icom).ne.ME) then
              Sbox => idis%box(:,:,src(icom))
              Dbox => odis%box(:,:,dst(icom))
              call boxIntersection( Sbox, Dbox, Lbox, inters )
#ifdef MPI
              CALL MPI_WAIT( request(icom), status, ierr )
#endif
              K3 = (Lbox(1,3) - Dbox(1,3))*NDST(1)*NDST(2)*NSM
              do I3 = Lbox(1,3), Lbox(2,3)
                K2 = (Lbox(1,2) - Dbox(1,2))*NDST(1)*NSM*NSM
                do I2 = Lbox(1,2), Lbox(2,2)
                  K1 = (Lbox(1,1) - Dbox(1,1))*NSP + 1 + K2 + K3
                  do I1 = Lbox(1,1), Lbox(2,1)
                    DO NN= 1, NSP
                      fdst(K1) = rBuff(J1)
                      K1 = K1 + 1
                      J1 = J1 + 1
                    ENDDO
                  enddo
                  K2 = K2 + NDST(1)*NSM*NSM
                enddo
                K3 = K3 + NDST(1)*NDST(2)*NSM
              enddo
            endif
          enddo
        else if (itr.eq.0) then
          KS = 1
          KR = 1
!         From sequencial to sequencial or from clustered to clustered
          do icom= 1, ncom
            Sbox => idis%box(:,:,src(icom))
            Dbox => odis%box(:,:,dst(icom))
            call boxIntersection( Sbox, Dbox, Lbox, inters )
            Xsize = (Lbox(2,1)-Lbox(1,1)+1)*NSM
            Ysize = (Lbox(2,2)-Lbox(1,2)+1)*NSM
            Zsize = (Lbox(2,3)-Lbox(1,3)+1)*NSM

            if (src(icom).eq.ME) then
              if (dst(icom).eq.ME) then
!               SRC and DST are the current process
                J3 = (Lbox(1,3) - Sbox(1,3))*NSRC(1)*NSRC(2)*NSM
                K3 = (Lbox(1,3) - Dbox(1,3))*NDST(1)*NDST(2)*NSM
                do I3 = 1, Zsize
                  J2 = (Lbox(1,2) - Sbox(1,2))*NSRC(1)*NSM
                  K2 = (Lbox(1,2) - Dbox(1,2))*NDST(1)*NSM
                  do I2 = 1, Ysize
                    J1 = (Lbox(1,1) - Sbox(1,1))*NSM + 1 + J2 + J3
                    K1 = (Lbox(1,1) - Dbox(1,1))*NSM + 1 + K2 + K3
                    do I1 = 1, Xsize
                      fdst(K1) = fsrc(J1)
                      K1 = K1 + 1
                      J1 = J1 + 1
                    enddo
                    J2 = J2 + NSRC(1)
                    K2 = K2 + NDST(1)
                  enddo
                  J3 = J3 + NSRC(1)*NSRC(2)
                  K3 = K3 + NDST(1)*NDST(2)
                enddo
              else
!               We should send data to process dst(icom)-1
                lsize = Xsize*Ysize*Zsize

                J3 = (Lbox(1,3) - Sbox(1,3))*NSRC(1)*NSRC(2)*NSM
                K1 = KS
                do I3 = 1, Zsize
                  J2 = (Lbox(1,2) - Sbox(1,2))*NSRC(1)*NSM
                  do I2 = 1, Ysize
                    J1 = (Lbox(1,1) - Sbox(1,1))*NSM + 1 + J2 + J3
                    do I1 = 1, Xsize
                      sBuff(K1) = fsrc(J1)
                      K1 = K1 + 1
                      J1 = J1 + 1
                    enddo
                    J2 = J2 + NSRC(1)
                  enddo
                  J3 = J3 + NSRC(1)*NSRC(2)
                enddo
#ifdef MPI
                call mpi_isend( sBuff(KS), lsize, MPI_grid_real,
     &                          dst(icom)-1, 0, MPI_COMM_WORLD,
     &                          request(icom), ierr )
#endif
                KS = K1
              endif
            else
!             We should receive data from process src(icom)-1
              lsize = Xsize*Ysize*Zsize
#ifdef MPI
              call mpi_irecv( rBuff(KR), lsize, MPI_grid_real,
     &                        src(icom)-1, 0, MPI_COMM_WORLD,
     &                        request(icom), ierr )
#endif
              KR = KR + lsize
            endif
          enddo

          J1 = 1
          do icom= 1, ncom
!           Wait for received data and move it to the destination buffer
            if (src(icom).ne.ME) then
              Sbox => idis%box(:,:,src(icom))
              Dbox => odis%box(:,:,dst(icom))
              call boxIntersection( Sbox, Dbox, Lbox, inters )
              Xsize = (Lbox(2,1)-Lbox(1,1)+1)*NSM
              Ysize = (Lbox(2,2)-Lbox(1,2)+1)*NSM
              Zsize = (Lbox(2,3)-Lbox(1,3)+1)*NSM
#ifdef MPI
              CALL MPI_WAIT( request(icom), status, ierr )
#endif
              K3 = (Lbox(1,3) - Dbox(1,3))*NDST(1)*NDST(2)*NSM
              do I3 = 1, Zsize
                K2 = (Lbox(1,2) - Dbox(1,2))*NDST(1)*NSM
                do I2 = 1, Ysize
                  K1 = (Lbox(1,1) - Dbox(1,1))*NSM + 1 + K2 + K3
                  do I1 = 1, Xsize
                    fdst(K1) = rBuff(J1)
                    K1 = K1 + 1
                    J1 = J1 + 1
                  enddo
                  K2 = K2 + NDST(1)
                enddo
                K3 = K3 + NDST(1)*NDST(2)
              enddo
            endif
          enddo
        else
          if (Node.eq.0) then
            write(*,*)'ERROR: Wrong parameter for function distMeshData'
          endif
          call die()
        endif

        call de_alloc( JS,      'JS',      'distmeshdata' )
        call de_alloc( request, 'request', 'distmeshdata' )
      endif

#ifdef _TRACE_
      call MPI_Barrier( MPI_Comm_World, ierr )
      call MPItrace_event( 1000, 0 )
#endif
      call timer( 'COMM_BSC', 2 )
#ifdef DEBUG
      call write_debug( '    POS distMeshData' )
#endif
      end subroutine distMeshData_rea
#else /* SYNCHRONOUS communications */
      subroutine distMeshData_rea( iDistr, fsrc, oDistr, fdst, itr )
      use mesh,    only : nsm, nmeshg
#ifdef MPI
      use mpi_siesta
#endif
      implicit none
!     Passed arguments
      integer,       intent(in) :: iDistr, oDistr, itr
      real(grid_p),  intent(in) :: fsrc(*)
      real(grid_p), intent(out) :: fdst(*)

!     Local variables
      character(len=*), parameter :: myName = moduName//'distMeshData '
      character(len=*), parameter :: errMsg = myName//'ERROR: '
      integer                     :: I1, I2, I3, J1, J2, J3, K1, K2, K3,
     &                               N1, N2, N3, NN, ind, ncom,
     &                               icom, NSP, NSRC(3), NDST(3), ME,
     &                               MaxSize, Xsize, Ysize, Zsize,
     &                               Lbox(2,3)
      integer, pointer            :: src(:), dst(:), JS(:), Sbox(:,:),
     &                               Dbox(:,:)
      type(meshDisType),  pointer :: idis, odis
      logical                     :: inters
      real(grid_p),       pointer :: TBUF(:)
      integer                     :: nsize, nm(3)
#ifdef MPI
      integer                     :: MPIerror, Status(MPI_Status_Size)
#endif
!----------------------------------------------------------------------- BEGIN
#ifdef DEBUG
      call write_debug( '    PRE distMeshData' )
#endif
#ifdef _TRACE_
      call MPI_Barrier( MPI_Comm_World, MPIerror )
      call MPItrace_event( 1000, 6 )
#endif
      call timer( 'COMM_BSC', 1 )

      if (nodes == 1) then
        nm(1:3) = nmeshg(1:3)/nsm
        if (itr.gt.0) then
           ! Note that in reord the first argument is always
           ! clustered
           call reord( fsrc, fdst, nm, nsm, TO_SEQUENTIAL )
        else if (itr .lt. 0) then
           call reord( fdst, fsrc, nm, nsm, TO_CLUSTER )
        else
           ! Copy source to destination 
           ! This will be executed only in serial mode,
           ! so we know that the size is the total number
           ! of (small) points, but maybe this information
           ! should be more explicit.
           nsize = product(nmeshg(1:3))
           fdst(1:nsize) = fsrc(1:nsize)
        endif

      else  ! nodes > 1
!       The communications are stored in a triangular structure.
        if (iDistr.gt.oDistr) then
          ind  = ((iDistr-1)*(iDistr-2))/2 + oDistr
          ncom = meshCommu(ind)%ncom
          src => meshCommu(ind)%dst
          dst => meshCommu(ind)%src
        else
          ind = ((oDistr-1)*(oDistr-2))/2 + iDistr
          ncom = meshCommu(ind)%ncom
          src => meshCommu(ind)%src
          dst => meshCommu(ind)%dst
        endif

        idis => meshDistr(iDistr)
        odis => meshDistr(oDistr)

        NSP = NSM*NSM*NSM
        ME  = Node + 1

        nullify( JS )
        call re_alloc( JS, 1, NSP, 'JS', 'moreMeshSubs' )

!       Compute the maximum size of the buffer needed to transfer data
!       among the several processes
        maxSize = 0
        do icom= 1, ncom
          if (src(icom).ne.dst(icom)) then
            Sbox => idis%box(:,:,src(icom))
            Dbox => odis%box(:,:,dst(icom))
            call boxIntersection( Sbox, Dbox, Lbox, inters )
            Xsize = Lbox(2,1) - Lbox(1,1) + 1
            Ysize = Lbox(2,2) - Lbox(1,2) + 1
            Zsize = Lbox(2,3) - Lbox(1,3) + 1
            MaxSize = max(MaxSize,Xsize*Ysize*Zsize)
          endif
        enddo

        MaxSize = MaxSize*nsp
        if (MaxSize.gt.0) then
          nullify( TBUF )
          call re_alloc( TBUF, 1, MaxSize, 'TBUF', 'moreMeshSubs' )
        endif

        Sbox => idis%box(:,:,ME)
        NSRC(1) = (Sbox(2,1) - Sbox(1,1) + 1)*nsm
        NSRC(2) = (Sbox(2,2) - Sbox(1,2) + 1)*nsm
        NSRC(3) = (Sbox(2,3) - Sbox(1,3) + 1)*nsm
        Dbox => odis%box(:,:,ME)
        NDST(1) = (Dbox(2,1) - Dbox(1,1) + 1)*nsm
        NDST(2) = (Dbox(2,2) - Dbox(1,2) + 1)*nsm
        NDST(3) = (Dbox(2,3) - Dbox(1,3) + 1)*nsm

        if (itr.eq.1) then
!         From clustered to sequential
          NN = 1
          I3 = 0
          DO N3=0, NSM-1
            I2 = 0
            DO N2= 0, NSM-1
              I1 = I2 + I3
              DO N1= 0, NSM-1
                JS(NN) = I1
                NN     = NN + 1
                I1     = I1 + 1
              ENDDO
              I2 = I2 + NDST(1)
            ENDDO
            I3 = I3 + NDST(1)*NDST(2)
          ENDDO
          do icom= 1, ncom
            Sbox => idis%box(:,:,src(icom))
            Dbox => odis%box(:,:,dst(icom))
            call boxIntersection( Sbox, Dbox, Lbox, inters )
            if (src(icom).eq.ME) then
              if (dst(icom).eq.ME) then
!               SRC and DST are the current process
                J3 = (Lbox(1,3) - Sbox(1,3))*NSRC(1)*NSRC(2)*NSM
                K3 = (Lbox(1,3) - Dbox(1,3))*NDST(1)*NDST(2)*NSM
                do I3 = Lbox(1,3), Lbox(2,3)
                  J2 = (Lbox(1,2) - Sbox(1,2))*NSRC(1)*NSM*NSM
                  K2 = (Lbox(1,2) - Dbox(1,2))*NDST(1)*NSM
                  do I2 = Lbox(1,2), Lbox(2,2)
                    J1 = (Lbox(1,1) - Sbox(1,1))*NSP + 1 + J2 + J3
                    K1 = (Lbox(1,1) - Dbox(1,1))*NSM + 1 + K2 + K3
                    do I1 = Lbox(1,1), Lbox(2,1)
                      DO NN= 1, NSP
                        fdst(K1+JS(NN)) = fsrc(J1)
                        J1 = J1 + 1
                      ENDDO
                      K1 = K1 + NSM
                    enddo
                    J2 = J2 + NSRC(1)*NSM*NSM
                    K2 = K2 + NDST(1)*NSM
                  enddo
                  J3 = J3 + NSRC(1)*NSRC(2)*NSM
                  K3 = K3 + NDST(1)*NDST(2)*NSM
                enddo
              else
!               We should send data to process dst(icom)-1
                J3 = (Lbox(1,3) - Sbox(1,3))*NSRC(1)*NSRC(2)*NSM
                K1 = 1
                do I3 = Lbox(1,3), Lbox(2,3)
                  J2 = (Lbox(1,2) - Sbox(1,2))*NSRC(1)*NSM*NSM
                  do I2 = Lbox(1,2), Lbox(2,2)
                    J1 = (Lbox(1,1) - Sbox(1,1))*NSP + 1 + J2 + J3
                    do I1 = Lbox(1,1), Lbox(2,1)
                      DO NN= 1, NSP
                        TBUF(K1) = fsrc(J1)
                        J1 = J1 + 1
                        K1 = K1 + 1
                      ENDDO
                    enddo
                    J2 = J2 + NSRC(1)*NSM*NSM
                  enddo
                  J3 = J3 + NSRC(1)*NSRC(2)*NSM
                enddo

                Xsize = Lbox(2,1) - Lbox(1,1) + 1
                Ysize = Lbox(2,2) - Lbox(1,2) + 1
                Zsize = Lbox(2,3) - Lbox(1,3) + 1
#ifdef MPI
                call MPI_Send( TBUF, Xsize*Ysize*Zsize*nsp,
     &                         MPI_grid_real, dst(icom)-1, 1,
     &                         MPI_Comm_world, MPIerror )
#endif
              endif
            else
!             We should receive data from process src(icom)-1
              Xsize = Lbox(2,1) - Lbox(1,1) + 1
              Ysize = Lbox(2,2) - Lbox(1,2) + 1
              Zsize = Lbox(2,3) - Lbox(1,3) + 1
#ifdef MPI
              call mpi_recv( TBUF, Xsize*Ysize*Zsize*nsp,
     &                       MPI_grid_real, src(icom)-1, 1,
     &                       MPI_Comm_world, Status, MPIerror )
#endif

              J1 = 1
              K3 = (Lbox(1,3) - Dbox(1,3))*NDST(1)*NDST(2)*NSM
              do I3 = Lbox(1,3), Lbox(2,3)
                K2 = (Lbox(1,2) - Dbox(1,2))*NDST(1)*NSM
                do I2 = Lbox(1,2), Lbox(2,2)
                  K1 = (Lbox(1,1) - Dbox(1,1))*NSM + 1 + K2 + K3
                  do I1 = Lbox(1,1), Lbox(2,1)
                    DO NN= 1, NSP
                      fdst(K1+JS(NN)) = TBUF(J1)
                      J1 = J1 + 1
                    ENDDO
                    K1 = K1 + NSM
                  enddo
                  K2 = K2 + NDST(1)*NSM
                enddo
                K3 = K3 + NDST(1)*NDST(2)*NSM
              enddo
            endif
          enddo
        else if (itr.eq.-1) then
!         From sequencial to clustered
          NN = 1
          I3 = 0
          DO N3=0, NSM-1
            I2 = 0
            DO N2= 0, NSM-1
              I1 = I2 + I3
              DO N1= 0, NSM-1
                JS(NN) = I1
                NN     = NN + 1
                I1     = I1 + 1
              ENDDO
              I2 = I2 + NSRC(1)
            ENDDO
            I3 = I3 + NSRC(1)*NSRC(2)
          ENDDO
          do icom= 1, ncom
            Sbox => idis%box(:,:,src(icom))
            Dbox => odis%box(:,:,dst(icom))
            call boxIntersection( Sbox, Dbox, Lbox, inters )
            if (src(icom).eq.ME) then
              if (dst(icom).eq.ME) then
!               SRC and DST are the current process
                J3 = (Lbox(1,3) - Sbox(1,3))*NSRC(1)*NSRC(2)*NSM
                K3 = (Lbox(1,3) - Dbox(1,3))*NDST(1)*NDST(2)*NSM
                do I3 = Lbox(1,3), Lbox(2,3)
                  J2 = (Lbox(1,2) - Sbox(1,2))*NSRC(1)*NSM
                  K2 = (Lbox(1,2) - Dbox(1,2))*NDST(1)*NSM*NSM
                  do I2 = Lbox(1,2), Lbox(2,2)
                    J1 = (Lbox(1,1) - Sbox(1,1))*NSM + 1 + J2 + J3
                    K1 = (Lbox(1,1) - Dbox(1,1))*NSP + 1 + K2 + K3
                    do I1 = Lbox(1,1), Lbox(2,1)
                      DO NN= 1, NSP
                        fdst(K1) = fsrc(J1+JS(NN))
                        K1 = K1 + 1
                      ENDDO
                      J1 = J1 + NSM
                    enddo
                    J2 = J2 + NSRC(1)*NSM
                    K2 = K2 + NDST(1)*NSM*NSM
                  enddo
                  J3 = J3 + NSRC(1)*NSRC(2)*NSM
                  K3 = K3 + NDST(1)*NDST(2)*NSM
                enddo
              else
!               We should send data to process dst(icom)-1
                J3 = (Lbox(1,3) - Sbox(1,3))*NSRC(1)*NSRC(2)*NSM
                K1 = 1
                do I3 = Lbox(1,3), Lbox(2,3)
                  J2 = (Lbox(1,2) - Sbox(1,2))*NSRC(1)*NSM
                  do I2 = Lbox(1,2), Lbox(2,2)
                    J1 = (Lbox(1,1) - Sbox(1,1))*NSM + 1 + J2 + J3
                    do I1 = Lbox(1,1), Lbox(2,1)
                      DO NN= 1, NSP
                        TBUF(K1) = fsrc(J1+JS(NN))
                        K1 = K1 + 1
                      ENDDO
                      J1 = J1 + NSM
                    enddo
                    J2 = J2 + NSRC(1)*NSM
                  enddo
                  J3 = J3 + NSRC(1)*NSRC(2)*NSM
                enddo

                Xsize = Lbox(2,1) - Lbox(1,1) + 1
                Ysize = Lbox(2,2) - Lbox(1,2) + 1
                Zsize = Lbox(2,3) - Lbox(1,3) + 1
#ifdef MPI
                call MPI_Send( TBUF, Xsize*Ysize*Zsize*nsp,
     &                         MPI_grid_real, dst(icom)-1, 1,
     &                         MPI_Comm_world, MPIerror )
#endif
              endif
            else
!             We should receive data from process src(icom)-1
              Xsize = Lbox(2,1) - Lbox(1,1) + 1
              Ysize = Lbox(2,2) - Lbox(1,2) + 1
              Zsize = Lbox(2,3) - Lbox(1,3) + 1
#ifdef MPI
              call mpi_recv( TBUF, Xsize*Ysize*Zsize*nsp,
     &                       MPI_grid_real, src(icom)-1, 1,
     &                       MPI_Comm_world, Status, MPIerror )
#endif

              J1 = 1
              K3 = (Lbox(1,3) - Dbox(1,3))*NDST(1)*NDST(2)*NSM
              do I3 = Lbox(1,3), Lbox(2,3)
                K2 = (Lbox(1,2) - Dbox(1,2))*NDST(1)*NSM*NSM
                do I2 = Lbox(1,2), Lbox(2,2)
                  K1 = (Lbox(1,1) - Dbox(1,1))*NSP + 1 + K2 + K3
                  do I1 = Lbox(1,1), Lbox(2,1)
                    DO NN= 1, NSP
                      fdst(K1) = TBUF(J1)
                      K1 = K1 + 1
                      J1 = J1 + 1
                    ENDDO
                  enddo
                  K2 = K2 + NDST(1)*NSM*NSM
                enddo
                K3 = K3 + NDST(1)*NDST(2)*NSM
              enddo
            endif
          enddo
        else if (itr.eq.0) then
!         From sequencial to sequencial or from clustered to clustered
          do icom= 1, ncom
            Sbox => idis%box(:,:,src(icom))
            Dbox => odis%box(:,:,dst(icom))
            call boxIntersection( Sbox, Dbox, Lbox, inters )
            Xsize = (Lbox(2,1) - Lbox(1,1) + 1)*NSM
            Ysize = (Lbox(2,2) - Lbox(1,2) + 1)*NSM
            Zsize = (Lbox(2,3) - Lbox(1,3) + 1)*NSM
            if (src(icom).eq.ME) then
              if (dst(icom).eq.ME) then
!               SRC and DST are the current process
                J3 = (Lbox(1,3) - Sbox(1,3))*NSRC(1)*NSRC(2)*NSM
                K3 = (Lbox(1,3) - Dbox(1,3))*NDST(1)*NDST(2)*NSM
                do I3 = 1, Zsize
                  J2 = (Lbox(1,2) - Sbox(1,2))*NSRC(1)*NSM
                  K2 = (Lbox(1,2) - Dbox(1,2))*NDST(1)*NSM
                  do I2 = 1, Ysize
                    J1 = (Lbox(1,1) - Sbox(1,1))*NSM + 1 + J2 + J3
                    K1 = (Lbox(1,1) - Dbox(1,1))*NSM + 1 + K2 + K3
                    do I1 = 1, Xsize
                      fdst(K1) = fsrc(J1)
                      K1 = K1 + 1
                      J1 = J1 + 1
                    enddo
                    J2 = J2 + NSRC(1)
                    K2 = K2 + NDST(1)
                  enddo
                  J3 = J3 + NSRC(1)*NSRC(2)
                  K3 = K3 + NDST(1)*NDST(2)
                enddo
              else
!               We should send data to process dst(icom)-1
                J3 = (Lbox(1,3) - Sbox(1,3))*NSRC(1)*NSRC(2)*NSM
                K1 = 1
                do I3 = 1, Zsize
                  J2 = (Lbox(1,2) - Sbox(1,2))*NSRC(1)*NSM
                  do I2 = 1, Ysize
                    J1 = (Lbox(1,1) - Sbox(1,1))*NSM + 1 + J2 + J3
                    do I1 = 1, Xsize
                      TBUF(K1) = fsrc(J1)
                      K1 = K1 + 1
                      J1 = J1 + 1
                    enddo
                    J2 = J2 + NSRC(1)
                  enddo
                  J3 = J3 + NSRC(1)*NSRC(2)
                enddo
#ifdef MPI
                call MPI_Send( TBUF, Xsize*Ysize*Zsize,
     &                         MPI_grid_real, dst(icom)-1, 1,
     &                         MPI_Comm_world, MPIerror )
#endif
              endif
            else
!             We should receive data from process src(icom)-1
#ifdef MPI
              call mpi_recv( TBUF, Xsize*Ysize*Zsize,
     &                       MPI_grid_real, src(icom)-1, 1,
     &                       MPI_Comm_world, Status, MPIerror )
#endif
              J1 = 1
              K3 = (Lbox(1,3) - Dbox(1,3))*NDST(1)*NDST(2)*NSM
              do I3 = 1, Zsize
                K2 = (Lbox(1,2) - Dbox(1,2))*NDST(1)*NSM
                do I2 = 1, Ysize
                  K1 = (Lbox(1,1) - Dbox(1,1))*NSM + 1 + K2 + K3
                  do I1 = 1, Xsize
                    fdst(K1) = TBUF(J1)
                    K1 = K1 + 1
                    J1 = J1 + 1
                  enddo
                  K2 = K2 + NDST(1)
                enddo
                K3 = K3 + NDST(1)*NDST(2)
              enddo
            endif
          enddo
        else
          if (Node.eq.0) then
            write(*,*)'ERROR: Wrong parameter for function distMeshData'
          endif
          call die()
        endif

        if (MaxSize.gt.0) then
          call de_alloc( TBUF, 'TBUF', 'moreMeshSubs' )
        endif

        call de_alloc( JS, 'JS', 'moreMeshSubs' )
      endif

#ifdef _TRACE_
      call MPI_Barrier( MPI_Comm_World, MPIerror )
      call MPItrace_event( 1000, 0 )
#endif
      call timer( 'COMM_BSC', 2 )
#ifdef DEBUG
      call write_debug( '    POS distMeshData' )
#endif
      end subroutine distMeshData_rea
#endif


      subroutine distMeshData_int( iDistr, fsrc, oDistr, fdst, itr )
#ifdef MPI
      use mpi_siesta
#endif
      implicit none
!     Passed arguments
      integer,  intent(in) :: iDistr, oDistr, itr
      integer,  intent(in) :: fsrc(*)
      integer, intent(out) :: fdst(*)

!     Local variables
      character(len=*), parameter :: myName = moduName//'distMeshData '
      character(len=*), parameter :: errMsg = myName//'ERROR: '
      integer                     :: I1, I2, I3, J1, J2, J3, K1, K2, K3,
     &                               ind, ncom, icom, NSRC(3), NDST(3),
     &                               ME, MaxSize, Xsize, Ysize, Zsize,
     &                               Lbox(2,3)
      integer, pointer            :: src(:), dst(:), Sbox(:,:),
     &                               Dbox(:,:)
      type(meshDisType),  pointer :: idis, odis
      logical                     :: inters
      integer,            pointer :: TBUF(:)
#ifdef MPI
      integer                     :: MPIerror, Status(MPI_Status_Size)
#endif

      if (nodes == 1) then
         call die("Called _int version of distMeshData for n=1")
      else
!       The communications are stored in a triangular structure.
        if (iDistr.gt.oDistr) then
          ind  = ((iDistr-1)*(iDistr-2))/2 + oDistr
          ncom = meshCommu(ind)%ncom
          src => meshCommu(ind)%dst
          dst => meshCommu(ind)%src
        else
          ind = ((oDistr-1)*(oDistr-2))/2 + iDistr
          ncom = meshCommu(ind)%ncom
          src => meshCommu(ind)%src
          dst => meshCommu(ind)%dst
        endif

        idis => meshDistr(iDistr)
        odis => meshDistr(oDistr)

        ME  = Node + 1

!       Compute the maximum size of the buffer needed to transfer data
!       among the several processes
        maxSize = 0
        do icom= 1, ncom
          if (src(icom).ne.dst(icom)) then
            Sbox => idis%box(:,:,src(icom))
            Dbox => odis%box(:,:,dst(icom))
            call boxIntersection( Sbox, Dbox, Lbox, inters )
            Xsize = Lbox(2,1) - Lbox(1,1) + 1
            Ysize = Lbox(2,2) - Lbox(1,2) + 1
            Zsize = Lbox(2,3) - Lbox(1,3) + 1
            MaxSize = max(MaxSize,Xsize*Ysize*Zsize)
          endif
        enddo

        if (MaxSize.gt.0) then
          nullify( TBUF )
          call re_alloc( TBUF, 1, MaxSize, 'TBUF', 'moreMeshSubs' )
        endif

        Sbox => idis%box(:,:,ME)
        NSRC(1) = Sbox(2,1) - Sbox(1,1) + 1
        NSRC(2) = Sbox(2,2) - Sbox(1,2) + 1
        NSRC(3) = Sbox(2,3) - Sbox(1,3) + 1
        Dbox => odis%box(:,:,ME)
        NDST(1) = Dbox(2,1) - Dbox(1,1) + 1
        NDST(2) = Dbox(2,2) - Dbox(1,2) + 1
        NDST(3) = Dbox(2,3) - Dbox(1,3) + 1

        if (itr.eq.0) then
!         From sequencial to sequencial
          do icom= 1, ncom
            Sbox => idis%box(:,:,src(icom))
            Dbox => odis%box(:,:,dst(icom))
            call boxIntersection( Sbox, Dbox, Lbox, inters )
            Xsize = Lbox(2,1) - Lbox(1,1) + 1
            Ysize = Lbox(2,2) - Lbox(1,2) + 1
            Zsize = Lbox(2,3) - Lbox(1,3) + 1
            if (src(icom).eq.ME) then
              if (dst(icom).eq.ME) then
!               SRC and DST are the current process
                J3 = (Lbox(1,3) - Sbox(1,3))*NSRC(1)*NSRC(2)
                K3 = (Lbox(1,3) - Dbox(1,3))*NDST(1)*NDST(2)
                do I3 = 1, Zsize
                  J2 = (Lbox(1,2) - Sbox(1,2))*NSRC(1)
                  K2 = (Lbox(1,2) - Dbox(1,2))*NDST(1)
                  do I2 = 1, Ysize
                    J1 = Lbox(1,1) - Sbox(1,1) + 1 + J2 + J3
                    K1 = Lbox(1,1) - Dbox(1,1) + 1 + K2 + K3
                    do I1 = 1, Xsize
                      fdst(K1) = fsrc(J1)
                      K1 = K1 + 1
                      J1 = J1 + 1
                    enddo
                    J2 = J2 + NSRC(1)
                    K2 = K2 + NDST(1)
                  enddo
                  J3 = J3 + NSRC(1)*NSRC(2)
                  K3 = K3 + NDST(1)*NDST(2)
                enddo
              else
!               We should send data to process dst(icom)-1
                J3 = (Lbox(1,3) - Sbox(1,3))*NSRC(1)*NSRC(2)
                K1 = 1
                do I3 = 1, Zsize
                  J2 = (Lbox(1,2) - Sbox(1,2))*NSRC(1)
                  do I2 = 1, Ysize
                    J1 = Lbox(1,1) - Sbox(1,1) + 1 + J2 + J3
                    do I1 = 1, Xsize
                      TBUF(K1) = fsrc(J1)
                      K1 = K1 + 1
                      J1 = J1 + 1
                    enddo
                    J2 = J2 + NSRC(1)
                  enddo
                  J3 = J3 + NSRC(1)*NSRC(2)
                enddo
#ifdef MPI
                call MPI_Send( TBUF, Xsize*Ysize*Zsize,
     &                         MPI_Integer, dst(icom)-1, 1,
     &                         MPI_Comm_world, MPIerror )
#endif
              endif
            else
!             We should receive data from process src(icom)-1
#ifdef MPI
              call mpi_recv( TBUF, Xsize*Ysize*Zsize,
     &                       MPI_Integer, src(icom)-1, 1,
     &                       MPI_Comm_world, Status, MPIerror )
#endif
              J1 = 1
              K3 = (Lbox(1,3) - Dbox(1,3))*NDST(1)*NDST(2)
              do I3 = 1, Zsize
                K2 = (Lbox(1,2) - Dbox(1,2))*NDST(1)
                do I2 = 1, Ysize
                  K1 = Lbox(1,1) - Dbox(1,1) + 1 + K2 + K3
                  do I1 = 1, Xsize
                    fdst(K1) = TBUF(J1)
                    K1 = K1 + 1
                    J1 = J1 + 1
                  enddo
                  K2 = K2 + NDST(1)
                enddo
                K3 = K3 + NDST(1)*NDST(2)
              enddo
            endif
          enddo
        else
          if (Node.eq.0) then
            write(*,*)'ERROR: Wrong parameter for function distMeshData'
          endif
          call die()
        endif

        if (MaxSize.gt.0) then
          call de_alloc( TBUF, 'TBUF', 'moreMeshSubs' )
        endif
      endif

      end subroutine distMeshData_int


      subroutine boxIntersection( ibox1, ibox2, obox, inters )
!! Checks the three axis of the input boxes to see if there is
!! intersection between the input boxes. If it exists, returns
!! the resulting box.
      implicit none
!     Passed arguments
      integer,  intent(in) :: ibox1(2,3), ibox2(2,3)
        !! Input box
      integer, intent(out) :: obox(2,3)
        !! Intersection between `ibox1` and `ibox2`
      logical, intent(out) :: inters
        !! `TRUE`, if there is an intersection. Otherwise `FALSE`.

!     Local variables
      integer              :: iaxis

      inters = .true.
      do iaxis= 1, 3
        obox(1,iaxis) = max(ibox1(1,iaxis),ibox2(1,iaxis))
        obox(2,iaxis) = min(ibox1(2,iaxis),ibox2(2,iaxis))
        if (obox(2,iaxis).lt.obox(1,iaxis)) inters = .false.
      enddo

      end subroutine boxIntersection


#ifdef MPI
      subroutine initMeshExtencil( iDistr, nm )
!! Compute the needed communications in order to send/receive the
!! extencil (when the data is ordered in the distribution `iDistr`)
!! The results are stored in the variable
!! `[[moreMeshSubs(module):exteCommu(variable)]](iDistr,1:3)`
!! of the current module.
!!
!! For every dimension of the problem, search all the neighbors that
!! we have. Given the current data distribution we compute the limits
!! of our extencil and we check its intersection with all the other
!! processes. Once we know all our neighbors we call subroutine
!! `[[scheduleComm(proc)]]` in order to minimize the number
!! of communications steps.

      use scheComm
      implicit none
!     Passed arguments
      integer,  intent(in)        :: iDistr
        !! Distribution index to be used
      integer,  intent(in)        :: nm(3)
        !! Number of Mesh divisions in each cell vector

!     Local variables
      integer                     :: Ubox(2,3), Lbox(2,3), Ibox(2,3),
     &                               ii, iaxis, ncom, Gcom, Lcom, P1, P2
      integer,            pointer :: src(:), dst(:), Dbox(:,:)
      type(meshDisType),  pointer :: idis
      type(meshCommType), pointer :: mcomm
      type(COMM_T)                :: comm
      logical                     :: inters

      idis => meshDistr(iDistr)

      do iaxis=1, 3
!       One communication structure for every dimension
        mcomm => exteCommu(iDistr,iaxis)

!       Count the number of communications needed to send/receive
!       the extencil
        ncom = 0
        do P1= 1, Nodes
!         Create the extencil boxes for both sides of the current
!         partition
          Ubox(1:2,1:3) = idis%box(1:2,1:3,P1)
          Ubox(1,iaxis) = Ubox(1,iaxis) - 1
          if (Ubox(1,iaxis).lt.1) Ubox(1,iaxis) = nm(iaxis)
          Ubox(2,iaxis)   = Ubox(1,iaxis)

          Lbox(1:2,1:3) = idis%box(1:2,1:3,P1)
          Lbox(2,iaxis) = Lbox(2,iaxis) + 1
          if (Lbox(2,iaxis).gt.nm(iaxis)) Lbox(2,iaxis) = 1
          Lbox(1,iaxis) = Lbox(2,iaxis)

          do P2= P1+1, Nodes
            Dbox => idis%box(:,:,P2)
            call boxIntersection( Dbox, Ubox, Ibox, inters )
            if (inters) then
              ncom = ncom + 1
            else
              call boxIntersection( Dbox, Lbox, Ibox, inters )
              if (inters) ncom = ncom + 1
            endif
          enddo
        enddo

        Gcom = ncom
!       Create a list of communications needed to send/receive
!       the extencil
        if (Gcom.gt.0) then
          nullify( src, dst )
          call re_alloc( src, 1, Gcom, 'src', 'moreMeshSubs' )
          call re_alloc( dst, 1, Gcom, 'dst', 'moreMeshSubs' )

          ncom = 0
          do P1= 1, Nodes
            Ubox(1:2,1:3) = idis%box(1:2,1:3,P1)
            Ubox(1,iaxis) = Ubox(1,iaxis) - 1
            if (Ubox(1,iaxis).lt.1) Ubox(1,iaxis) = nm(iaxis)
            Ubox(2,iaxis)   = Ubox(1,iaxis)

            Lbox(1:2,1:3) = idis%box(1:2,1:3,P1)
            Lbox(2,iaxis) = Lbox(2,iaxis) + 1
            if (Lbox(2,iaxis).gt.nm(iaxis)) Lbox(2,iaxis) = 1
            Lbox(1,iaxis) = Lbox(2,iaxis)

            do P2= P1+1, Nodes
              Dbox => idis%box(:,:,P2)
              call boxIntersection( Dbox, Ubox, Ibox, inters )
              if (inters) then
                ncom = ncom + 1
                src(ncom) = P1
                dst(ncom) = P2
              else
                call boxIntersection( Dbox, Lbox, Ibox, inters )
                if (inters) then
                  ncom = ncom + 1
                  src(ncom) = P1
                  dst(ncom) = P2
                endif
              endif
            enddo
          enddo

          comm%np = Nodes
!         reschedule the communications in order to minimize the time
          call scheduleComm( Gcom, src, dst, comm )

!         Count the number of communications needed by the current process
          ncom = 0
          do P1= 1, comm%ncol
            if (comm%ind(P1,Node+1).ne.0) ncom = ncom + 1
          enddo
          Lcom = ncom

!         Store the ordered list of communications needed by the current
!         process to send/receive the extencil.
          if (Lcom.gt.0) then
            nullify( mcomm%src, mcomm%dst )
            call re_alloc( mcomm%src, 1, Lcom, 'mcomm%src',
     &                     'moreMeshSubs' )
            call re_alloc( mcomm%dst, 1, Lcom, 'mcomm%dst',
     &                     'moreMeshSubs' )

            ncom = 0
            do P1= 1, comm%ncol
              ii = comm%ind(P1,Node+1)
              if (ii.ne.0) then
                ncom            = ncom + 1
                mcomm%src(ncom) = src(ii)
                mcomm%dst(ncom) = dst(ii)
              endif
            enddo
            mcomm%ncom = Lcom

            call de_alloc( comm%ind, 'comm%ind', 'scheComm' )
          endif

          call de_alloc( dst, 'dst', 'moreMeshSubs' )
          call de_alloc( src, 'src', 'moreMeshSubs' )
        endif
      enddo

      end subroutine initMeshExtencil


      subroutine distExtMeshData( iDistr, iaxis, BS, NSM, NN, NSPIN,
     &                            maxp, NMeshG, dens, BDENS )
!! Send/receive the extencil information from the `DENS` matrix to the
!! temporal array `BDENS`.
!!
!! We have a different code for every axis. We should find if we
!! intersects with a neightbour node throught the upper, the lower
!! or both sides.

      use mpi_siesta
      implicit none

      integer,         intent(in) :: iDistr
        !! Distribution index to be used
      integer,         intent(in) :: iaxis
        !! Axe to be splitted
      integer,         intent(in) :: BS
        !! Dimension of a plane in the current axe
      integer,         intent(in) :: NSM
        !! Number of mesh sub-divisions in each direction
      integer,         intent(in) :: NN
        !! Size of the extencil
      integer,         intent(in) :: NSPIN
        !! Number of pollarizations
      integer,         intent(in) :: maxp
        !! Total number of points
      integer,         intent(in) :: NMeshG(3)
        !! Number of Mesh points in each cell vector
      real(gp),        intent(in) :: DENS(maxp,NSPIN)
        !! Electron density matrix
      real(gp),       intent(out) :: BDENS(BS,2*NN,NSPIN)
        !! Auxiliary arrays to store the extencil from other partitions

!     Local variables
      integer                     :: Ubox(2,3), Lbox(2,3), IUbox(2,3),
     &                               ILbox(2,3), nm(3), ispin, Cnode,
     &                               iniX, endX, iniY, endY, iniZ, endZ,
     &                               ix, iy, iz, tt, uu, dimB(3), ii, PP
      logical                     :: inter1, inter2
      integer,            pointer :: Dbox(:,:)
      real(gp),           pointer :: SBUF(:), RBUF(:)
      type(meshDisType),  pointer :: idis
      type(meshCommType), pointer :: mcomm
      integer                     :: MPIerror, Status(MPI_Status_Size)

      idis    => meshDistr(iDistr)
      mcomm   => exteCommu(iDistr,iaxis)
      nm      = NMeshG/NSM
      Cnode   = Node + 1
      dimB(1) = (idis%box(2,1,Cnode)-idis%box(1,1,Cnode)+1)*NSM
      dimB(2) = (idis%box(2,2,Cnode)-idis%box(1,2,Cnode)+1)*NSM
      dimB(3) = (idis%box(2,3,Cnode)-idis%box(1,3,Cnode)+1)*NSM

      if (.not.associated(mcomm%dst)) then
        write(6,*) 'ERROR: Trying to communicate extencil ',
     &             'with an uninitialized mesh distribution'
        call die()
      endif
      if (.not.associated(mcomm%src)) then
        write(6,*) 'ERROR: Trying to communicate extencil ',
     &             'with an uninitialized mesh distribution'
        call die()
      endif

      nullify(SBUF,RBUF)
      call re_alloc( SBUF, 1, BS*NN*nspin, 'SBUF', 'moreMeshSubs' )
      call re_alloc( RBUF, 1, BS*NN*nspin, 'RBUF', 'moreMeshSubs' )

      Ubox(1:2,1:3) = idis%box(1:2,1:3,Cnode)
      Ubox(1,iaxis) = Ubox(1,iaxis) - 1
      if (Ubox(1,iaxis).lt.1) Ubox(1,iaxis) = nm(iaxis)
      Ubox(2,iaxis)   = Ubox(1,iaxis)

      Lbox(1:2,1:3) = idis%box(1:2,1:3,Cnode)
      Lbox(2,iaxis) = Lbox(2,iaxis) + 1
      if (Lbox(2,iaxis).gt.nm(iaxis)) Lbox(2,iaxis) = 1
      Lbox(1,iaxis) = Lbox(2,iaxis)

      do ii= 1, mcomm%ncom
        if (Cnode.eq.mcomm%src(ii)) then
          PP = mcomm%dst(ii)
        else
          PP = mcomm%src(ii)
        endif
        Dbox => idis%box(:,:,PP)
        call boxIntersection( Dbox, Ubox, IUbox, inter1 )
        call boxIntersection( Dbox, Lbox, ILbox, inter2 )
        if (inter1) then
          if (iaxis.eq.1) then
            iniX = 1
            endX = NN
            iniY = (IUbox(1,2)-idis%box(1,2,Cnode))*NSM + 1
            endY = (IUbox(2,2)-idis%box(1,2,Cnode)+1)*NSM
            iniZ = (IUbox(1,3)-idis%box(1,3,Cnode))*NSM + 1
            endZ = (IUbox(2,3)-idis%box(1,3,Cnode)+1)*NSM
            tt = 0
            do ispin= 1, nspin
              do iz= iniZ, endZ
                do iy= iniY, endY
                  uu = (iz-1)*dimB(1)*dimB(2)+(iy-1)*dimB(1)+iniX
                  do ix= iniX, endX
                    tt = tt + 1
                    SBUF(tt) = dens(uu,ispin)
                    uu = uu + 1
                  enddo
                enddo
              enddo
            enddo
            call MPI_SendRecv( SBUF, tt, MPI_grid_real, PP-1, 0,
     &                         RBUF, tt, MPI_grid_real, PP-1, 0,
     &                         MPI_Comm_world, Status, MPIerror )
            if (inter2) then
              tt = 0
              do ispin= 1, nspin
                do iz= iniZ, endZ
                  uu = (iz-1)*dimB(2)+iniY
                  do iy= iniY, endY
                    do ix= 1, NN
                      tt = tt + 1
                      BDENS(uu,NN+ix,ispin) = RBUF(tt)
                    enddo
                    uu = uu + 1
                  enddo
                enddo
              enddo
            else
              tt = 0
              do ispin= 1, nspin
                do iz= iniZ, endZ
                  uu = (iz-1)*dimB(2)+iniY
                  do iy= iniY, endY
                    do ix= NN, 1, -1
                      tt = tt + 1
                      BDENS(uu,ix,ispin) = RBUF(tt)
                    enddo
                    uu = uu + 1
                  enddo
                enddo
              enddo
            endif
          else if (iaxis.eq.2) then
            iniX = (IUbox(1,1)-idis%box(1,1,Cnode))*NSM + 1
            endX = (IUbox(2,1)-idis%box(1,1,Cnode)+1)*NSM
            iniZ = (IUbox(1,3)-idis%box(1,3,Cnode))*NSM + 1
            endZ = (IUbox(2,3)-idis%box(1,3,Cnode)+1)*NSM
            tt = 0
            do ispin= 1, nspin
              do iz= iniZ, endZ
                do iy= 1, NN
                  uu = (iz-1)*dimB(1)*dimB(2)+(iy-1)*dimB(1)+iniX
                  do ix= iniX, endX
                    tt = tt + 1
                    SBUF(tt) = dens(uu,ispin)
                    uu = uu + 1
                  enddo
                enddo
              enddo
            enddo
            call MPI_SendRecv( SBUF, tt, MPI_grid_real, PP-1, 0,
     &                         RBUF, tt, MPI_grid_real, PP-1, 0,
     &                         MPI_Comm_world, Status, MPIerror )

            if (inter2) then
              tt = 0
              do ispin= 1, nspin
                do iz= iniZ, endZ
                  do iy= 1, NN
                    uu = (iz-1)*dimB(1)+iniX
                    do ix= iniX, endX
                      tt = tt + 1
                      BDENS(uu,NN+iy,ispin) = RBUF(tt)
                      uu = uu + 1
                    enddo
                  enddo
                enddo
              enddo
            else
              tt = 0
              do ispin= 1, nspin
                do iz= iniZ, endZ
                  do iy= NN, 1, -1
                    uu = (iz-1)*dimB(1)+iniX
                    do ix= iniX, endX
                      tt = tt + 1
                      BDENS(uu,iy,ispin) = RBUF(tt)
                      uu = uu + 1
                    enddo
                  enddo
                enddo
              enddo
            endif
          else
            iniX = (IUbox(1,1)-idis%box(1,1,Cnode))*NSM + 1
            endX = (IUbox(2,1)-idis%box(1,1,Cnode)+1)*NSM
            iniY = (IUbox(1,2)-idis%box(1,2,Cnode))*NSM + 1
            endY = (IUbox(2,2)-idis%box(1,2,Cnode)+1)*NSM
            tt = 0
            do ispin= 1, nspin
              do iz= 1, NN
                do iy= iniY, endY
                  uu = (iz-1)*dimB(1)*dimB(2)+(iy-1)*dimB(1)+iniX
                  do ix= iniX, endX
                    tt = tt + 1
                    SBUF(tt) = dens(uu,ispin)
                    uu = uu + 1
                  enddo
                enddo
              enddo
            enddo
            call MPI_SendRecv( SBUF, tt, MPI_grid_real, PP-1, 0,
     &                         RBUF, tt, MPI_grid_real, PP-1, 0,
     &                         MPI_Comm_world, Status, MPIerror )

            if (inter2) then
              tt = 0
              do ispin= 1, nspin
                do iz= 1, NN
                  do iy= iniY, endY
                    uu = (iy-1)*dimB(1)+iniX
                    do ix= iniX, endX
                      tt = tt + 1
                      BDENS(uu,NN+iz,ispin) = RBUF(tt)
                      uu = uu + 1
                    enddo
                  enddo
                enddo
              enddo
            else
              tt = 0
              do ispin= 1, nspin
                do iz= NN, 1, -1
                  do iy= iniY, endY
                    uu = (iy-1)*dimB(1)+iniX
                    do ix= iniX, endX
                      tt = tt + 1
                      BDENS(uu,iz,ispin) = RBUF(tt)
                      uu = uu + 1
                    enddo
                  enddo
                enddo
              enddo
            endif
          endif
        endif

        if (inter2) then
          if (iaxis.eq.1) then
            iniX = dimB(1)-NN+1
            endX = dimB(1)
            iniY = (ILbox(1,2)-idis%box(1,2,Cnode))*NSM + 1
            endY = (ILbox(2,2)-idis%box(1,2,Cnode)+1)*NSM
            iniZ = (ILbox(1,3)-idis%box(1,3,Cnode))*NSM + 1
            endZ = (ILbox(2,3)-idis%box(1,3,Cnode)+1)*NSM
            tt = 0
            do ispin= 1, nspin
              do iz= iniZ, endZ
                do iy= iniY, endY
                  uu = (iz-1)*dimB(1)*dimB(2)+(iy-1)*dimB(1)+iniX
                  do ix= iniX, endX
                    tt = tt + 1
                    SBUF(tt) = dens(uu,ispin)
                    uu = uu + 1
                  enddo
                enddo
              enddo
            enddo
            call MPI_SendRecv( SBUF, tt, MPI_grid_real, PP-1, 0,
     &                         RBUF, tt, MPI_grid_real, PP-1, 0,
     &                         MPI_Comm_world, Status, MPIerror )
            if (inter1) then
              tt = 0
              do ispin= 1, nspin
                do iz= iniZ, endZ
                  uu = (iz-1)*dimB(2)+iniY
                  do iy= iniY, endY
                    do ix= NN, 1, -1
                      tt = tt + 1
                      BDENS(uu,ix,ispin) = RBUF(tt)
                    enddo
                    uu = uu + 1
                  enddo
                enddo
              enddo
            else
              tt = 0
              do ispin= 1, nspin
                do iz= iniZ, endZ
                  uu = (iz-1)*dimB(2)+iniY
                  do iy= iniY, endY
                    do ix= 1, NN
                      tt = tt + 1
                      BDENS(uu,NN+ix,ispin) = RBUF(tt)
                    enddo
                    uu = uu + 1
                  enddo
                enddo
              enddo
            endif
          else if (iaxis.eq.2) then
            iniX = (ILbox(1,1)-idis%box(1,1,Cnode))*NSM + 1
            endX = (ILbox(2,1)-idis%box(1,1,Cnode)+1)*NSM
            iniZ = (ILbox(1,3)-idis%box(1,3,Cnode))*NSM + 1
            endZ = (ILbox(2,3)-idis%box(1,3,Cnode)+1)*NSM
            tt = 0
            do ispin= 1, nspin
              do iz= iniZ, endZ
                do iy= dimB(2)-NN+1, dimB(2)
                  uu = (iz-1)*dimB(1)*dimB(2)+(iy-1)*dimB(1)+iniX
                  do ix= iniX, endX
                    tt = tt + 1
                    SBUF(tt) = dens(uu,ispin)
                    uu = uu + 1
                  enddo
                enddo
              enddo
            enddo
            call MPI_SendRecv( SBUF, tt, MPI_grid_real, PP-1, 0,
     &                         RBUF, tt, MPI_grid_real, PP-1, 0,
     &                         MPI_Comm_world, Status, MPIerror )
            if (inter1) then
              tt = 0
              do ispin= 1, nspin
                do iz= iniZ, endZ
                  do iy= NN, 1, -1
                    uu = (iz-1)*dimB(1)+iniX
                    do ix= iniX, endX
                      tt = tt + 1
                      BDENS(uu,iy,ispin) = RBUF(tt)
                      uu = uu + 1
                    enddo
                  enddo
                enddo
              enddo
            else
              tt = 0
              do ispin= 1, nspin
                do iz= iniZ, endZ
                  do iy= 1, NN
                    uu = (iz-1)*dimB(1)+iniX
                    do ix= iniX, endX
                      tt = tt + 1
                      BDENS(uu,NN+iy,ispin) = RBUF(tt)
                      uu = uu + 1
                    enddo
                  enddo
                enddo
              enddo
            endif

          else
            iniX = (ILbox(1,1)-idis%box(1,1,Cnode))*NSM + 1
            endX = (ILbox(2,1)-idis%box(1,1,Cnode)+1)*NSM
            iniY = (ILbox(1,2)-idis%box(1,2,Cnode))*NSM + 1
            endY = (ILbox(2,2)-idis%box(1,2,Cnode)+1)*NSM
            tt = 0
            do ispin= 1, nspin
              do iz= dimB(3)-NN+1, dimB(3)
                do iy= iniY, endY
                  uu = (iz-1)*dimB(1)*dimB(2)+(iy-1)*dimB(1)+iniX
                  do ix= iniX, endX
                    tt = tt + 1
                    SBUF(tt) = dens(uu,ispin)
                    uu = uu + 1
                  enddo
                enddo
              enddo
            enddo
            call MPI_SendRecv( SBUF, tt, MPI_grid_real, PP-1, 0,
     &                         RBUF, tt, MPI_grid_real, PP-1, 0,
     &                         MPI_Comm_world, Status, MPIerror )
            if (inter1) then
              tt = 0
              do ispin= 1, nspin
                do iz= NN, 1, -1
                  do iy= iniY, endY
                    uu = (iy-1)*dimB(1)+iniX
                    do ix= iniX, endX
                      tt = tt + 1
                      BDENS(uu,iz,ispin) = RBUF(tt)
                      uu = uu + 1
                    enddo
                  enddo
                enddo
              enddo
            else
              tt = 0
              do ispin= 1, nspin
                do iz= 1, NN
                  do iy= iniY, endY
                    uu = (iy-1)*dimB(1)+iniX
                    do ix= iniX, endX
                      tt = tt + 1
                      BDENS(uu,NN+iz,ispin) = RBUF(tt)
                      uu = uu + 1
                    enddo
                  enddo
                enddo
              enddo
            endif

          endif
        endif
      enddo

      call de_alloc( RBUF, 'RBUF', 'moreMeshSubs' )
      call de_alloc( SBUF, 'SBUF', 'moreMeshSubs' )

      end subroutine distExtMeshData


      subroutine gathExtMeshData( iDistr, iaxis, BS, NSM, NN, NSPIN,
     &                            maxp, NMeshG, BVXC, VXC )
!! Send/receive the extencil information from the `BVXC` temporal array
!! to the array `VXC`.
!!
!! We have a different code for every axis. We should find if we
!! intersects with a neightbour node throught the upper, the lower
!! or both sides.

      use mpi_siesta
      implicit none

      integer,         intent(in) :: iDistr
        !! Distribution index to be used
      integer,         intent(in) :: iaxis
        !! Axe to be splitted
      integer,         intent(in) :: BS
        !! Dimension of a plane in the current axe
      integer,         intent(in) :: NSM
        !! Number of mesh sub-divisions in each direction
      integer,         intent(in) :: NN
        !! Size of the extencil
      integer,         intent(in) :: NSPIN
        !! Number of pollarizations
      integer,         intent(in) :: maxp
        !! Total number of points
      integer,         intent(in) :: NMeshG(3)
        !! Number of Mesh points in each cell vector
      real(gp),        intent(in) :: BVXC(BS,2*NN,NSPIN)
        !! Auxiliar array that contains the extencil of the
        !! exch-corr potential
      real(gp),       intent(out) :: VXC(maxp,NSPIN)
        !! Exch-corr potential

!     Local variables
      integer                     :: Ubox(2,3), Lbox(2,3), IUbox(2,3),
     &                               ILbox(2,3), nm(3), ispin, Cnode,
     &                               iniX, endX, iniY, endY, iniZ, endZ,
     &                               ix, iy, iz, tt, uu, dimB(3), ii, PP
      logical                     :: inter1, inter2
      integer,            pointer :: Dbox(:,:)
      real(gp),           pointer :: SBUF(:), RBUF(:)
      type(meshDisType),  pointer :: idis
      type(meshCommType), pointer :: mcomm
      integer                     :: MPIerror, Status(MPI_Status_Size)

      idis    => meshDistr(iDistr)
      mcomm   => exteCommu(iDistr,iaxis)
      nm      = NMeshG/NSM
      Cnode   = Node + 1
      dimB(1) = (idis%box(2,1,Cnode)-idis%box(1,1,Cnode)+1)*NSM
      dimB(2) = (idis%box(2,2,Cnode)-idis%box(1,2,Cnode)+1)*NSM
      dimB(3) = (idis%box(2,3,Cnode)-idis%box(1,3,Cnode)+1)*NSM

      nullify(SBUF,RBUF)
      call re_alloc( SBUF, 1, BS*NN*nspin, 'SBUF', 'moreMeshSubs' )
      call re_alloc( RBUF, 1, BS*NN*nspin, 'RBUF', 'moreMeshSubs' )

      Ubox(1:2,1:3) = idis%box(1:2,1:3,Cnode)
      Ubox(1,iaxis) = Ubox(1,iaxis) - 1
      if (Ubox(1,iaxis).lt.1) Ubox(1,iaxis) = nm(iaxis)
      Ubox(2,iaxis)   = Ubox(1,iaxis)

      Lbox(1:2,1:3) = idis%box(1:2,1:3,Cnode)
      Lbox(2,iaxis) = Lbox(2,iaxis) + 1
      if (Lbox(2,iaxis).gt.nm(iaxis)) Lbox(2,iaxis) = 1
      Lbox(1,iaxis) = Lbox(2,iaxis)

      do ii= 1, mcomm%ncom
        if (Cnode.eq.mcomm%src(ii)) then
          PP = mcomm%dst(ii)
        else
          PP = mcomm%src(ii)
        endif
        Dbox => idis%box(:,:,PP)
        call boxIntersection( Dbox, Ubox, IUbox, inter1 )
        call boxIntersection( Dbox, Lbox, ILbox, inter2 )
        if (inter1) then
          if (iaxis.eq.1) then
            iniX = 1
            endX = NN
            iniY = (IUbox(1,2)-idis%box(1,2,Cnode))*NSM + 1
            endY = (IUbox(2,2)-idis%box(1,2,Cnode)+1)*NSM
            iniZ = (IUbox(1,3)-idis%box(1,3,Cnode))*NSM + 1
            endZ = (IUbox(2,3)-idis%box(1,3,Cnode)+1)*NSM
            tt = 0
            do ispin= 1, nspin
              do ix= 1, NN
                do iz= iniZ, endZ
                  uu = (iz-1)*dimB(2)+iniY
                  do iy= iniY, endY
                    tt = tt + 1
                    SBUF(tt) = BVXC(uu,ix,ispin)
                    uu = uu + 1
                  enddo
                enddo
              enddo
            enddo
            call MPI_SendRecv( SBUF, tt, MPI_grid_real, PP-1, 0,
     &                         RBUF, tt, MPI_grid_real, PP-1, 0,
     &                         MPI_Comm_world, Status, MPIerror )

            if (inter2) then
              tt = 0
              do ispin= 1, nspin
                do ix= dimB(1), dimB(1)-NN+1, -1
                  do iz= iniZ, endZ
                    uu = (iz-1)*dimB(1)*dimB(2)+(iniY-1)*dimB(1)+ix
                    do iy= iniY, endY
                      tt = tt + 1
                      VXC(uu,ispin) = VXC(uu,ispin) + RBUF(tt)
                      uu = uu + dimB(1)
                    enddo
                  enddo
                enddo
              enddo
            else
              tt = 0
              do ispin= 1, nspin
                do ix=  1, NN
                  do iz= iniZ, endZ
                    uu = (iz-1)*dimB(1)*dimB(2)+(iniY-1)*dimB(1)+ix
                    do iy= iniY, endY
                      tt = tt + 1
                      VXC(uu,ispin) = VXC(uu,ispin) + RBUF(tt)
                      uu = uu + dimB(1)
                    enddo
                  enddo
                enddo
              enddo
            endif
          else if (iaxis.eq.2) then
            iniX = (IUbox(1,1)-idis%box(1,1,Cnode))*NSM + 1
            endX = (IUbox(2,1)-idis%box(1,1,Cnode)+1)*NSM
            iniZ = (IUbox(1,3)-idis%box(1,3,Cnode))*NSM + 1
            endZ = (IUbox(2,3)-idis%box(1,3,Cnode)+1)*NSM
            tt = 0
            do ispin= 1, nspin
              do iy= 1, NN
                do iz= iniZ, endZ
                  uu = (iz-1)*dimB(1)+iniX
                  do ix= iniX, endX
                    tt = tt + 1
                    SBUF(tt) = BVXC(uu,iy,ispin)
                    uu = uu + 1
                  enddo
                enddo
              enddo
            enddo
            call MPI_SendRecv( SBUF, tt, MPI_grid_real, PP-1, 0,
     &                         RBUF, tt, MPI_grid_real, PP-1, 0,
     &                         MPI_Comm_world, Status, MPIerror )
            if (inter2) then
              tt = 0
              do ispin= 1, nspin
                do iy= dimB(2), dimB(2)-NN+1, -1
                  do iz= iniZ, endZ
                    uu = (iz-1)*dimB(1)*dimB(2)+(iy-1)*dimB(1)+iniX
                    do ix= iniX, endX
                      tt = tt + 1
                      VXC(uu,ispin) = VXC(uu,ispin) + RBUF(tt)
                      uu = uu + 1
                    enddo
                  enddo
                enddo
              enddo
            else
              tt = 0
              do ispin= 1, nspin
                do iy=  1, NN
                  do iz= iniZ, endZ
                    uu = (iz-1)*dimB(1)*dimB(2)+(iy-1)*dimB(1)+iniX
                    do ix= iniX, endX
                      tt = tt + 1
                      VXC(uu,ispin) = VXC(uu,ispin) + RBUF(tt)
                      uu = uu + 1
                    enddo
                  enddo
                enddo
              enddo
            endif
          else
            iniX = (IUbox(1,1)-idis%box(1,1,Cnode))*NSM + 1
            endX = (IUbox(2,1)-idis%box(1,1,Cnode)+1)*NSM
            iniY = (IUbox(1,2)-idis%box(1,2,Cnode))*NSM + 1
            endY = (IUbox(2,2)-idis%box(1,2,Cnode)+1)*NSM
            tt = 0
            do ispin= 1, nspin
              do iz= 1, NN
                do iy= iniY, endY
                  uu = (iy-1)*dimB(1)+iniX
                  do ix= iniX, endX
                    tt = tt + 1
                    SBUF(tt) = BVXC(uu,iz,ispin)
                    uu = uu + 1
                  enddo
                enddo
              enddo
            enddo
            call MPI_SendRecv( SBUF, tt, MPI_grid_real, PP-1, 0,
     &                         RBUF, tt, MPI_grid_real, PP-1, 0,
     &                         MPI_Comm_world, Status, MPIerror )
            if (inter2) then
              tt = 0
              do ispin= 1, nspin
                do iz=  dimB(3), dimB(3)-NN+1, -1
                  do iy= iniY, endY
                    uu = (iz-1)*dimB(1)*dimB(2)+(iy-1)*dimB(1)+iniX
                    do ix= iniX, endX
                      tt = tt + 1
                      VXC(uu,ispin) = VXC(uu,ispin) + RBUF(tt)
                      uu = uu + 1
                    enddo
                  enddo
                enddo
              enddo
            else
              tt = 0
              do ispin= 1, nspin
                do iz=  1, NN
                  do iy= iniY, endY
                    uu = (iz-1)*dimB(1)*dimB(2)+(iy-1)*dimB(1)+iniX
                    do ix= iniX, endX
                      tt = tt + 1
                      VXC(uu,ispin) = VXC(uu,ispin) + RBUF(tt)
                      uu = uu + 1
                    enddo
                  enddo
                enddo
              enddo
            endif
          endif
        endif

        if (inter2) then
          if (iaxis.eq.1) then
            iniX = dimB(1)-NN+1
            endX = dimB(1)
            iniY = (ILbox(1,2)-idis%box(1,2,Cnode))*NSM + 1
            endY = (ILbox(2,2)-idis%box(1,2,Cnode)+1)*NSM
            iniZ = (ILbox(1,3)-idis%box(1,3,Cnode))*NSM + 1
            endZ = (ILbox(2,3)-idis%box(1,3,Cnode)+1)*NSM
            tt = 0
            do ispin= 1, nspin
              do ix= NN+1, 2*NN
                do iz= iniZ, endZ
                  uu = (iz-1)*dimB(2)+iniY
                  do iy= iniY, endY
                    tt = tt + 1
                    SBUF(tt) = BVXC(uu,ix,ispin)
                    uu = uu + 1
                  enddo
                enddo
              enddo
            enddo
            call MPI_SendRecv( SBUF, tt, MPI_grid_real, PP-1, 0,
     &                         RBUF, tt, MPI_grid_real, PP-1, 0,
     &                         MPI_Comm_world, Status, MPIerror )
            if (inter1) then
              tt = 0
              do ispin= 1, nspin
                do ix=  1, NN
                  do iz= iniZ, endZ
                    uu = (iz-1)*dimB(1)*dimB(2)+(iniY-1)*dimB(1)+ix
                    do iy= iniY, endY
                      tt = tt + 1
                      VXC(uu,ispin) = VXC(uu,ispin) + RBUF(tt)
                      uu = uu + dimB(1)
                    enddo
                  enddo
                enddo
              enddo
            else
              tt = 0
              do ispin= 1, nspin
                do ix= dimB(1), dimB(1)-NN+1, -1
                  do iz= iniZ, endZ
                    uu = (iz-1)*dimB(1)*dimB(2)+(iniY-1)*dimB(1)+ix
                    do iy= iniY, endY
                      tt = tt + 1
                      VXC(uu,ispin) = VXC(uu,ispin) + RBUF(tt)
                      uu = uu + dimB(1)
                    enddo
                  enddo
                enddo
              enddo
            endif
          else if (iaxis.eq.2) then
            iniX = (ILbox(1,1)-idis%box(1,1,Cnode))*NSM + 1
            endX = (ILbox(2,1)-idis%box(1,1,Cnode)+1)*NSM
            iniZ = (ILbox(1,3)-idis%box(1,3,Cnode))*NSM + 1
            endZ = (ILbox(2,3)-idis%box(1,3,Cnode)+1)*NSM
            tt = 0
            do ispin= 1, nspin
              do iy= NN+1, 2*NN
                do iz= iniZ, endZ
                  uu = (iz-1)*dimB(1)+iniX
                  do ix= iniX, endX
                    tt = tt + 1
                    SBUF(tt) = BVXC(uu,iy,ispin)
                    uu = uu + 1
                  enddo
                enddo
              enddo
            enddo
            call MPI_SendRecv( SBUF, tt, MPI_grid_real, PP-1, 0,
     &                         RBUF, tt, MPI_grid_real, PP-1, 0,
     &                         MPI_Comm_world, Status, MPIerror )
            if (inter1) then
              tt = 0
              do ispin= 1, nspin
                do iy= 1, NN
                  do iz= iniZ, endZ
                    uu = (iz-1)*dimB(1)*dimB(2)+(iy-1)*dimB(1)+iniX
                    do ix= iniX, endX
                      tt = tt + 1
                      VXC(uu,ispin) = VXC(uu,ispin) + RBUF(tt)
                      uu = uu + 1
                    enddo
                  enddo
                enddo
              enddo
            else
              tt = 0
              do ispin= 1, nspin
                do iy= dimB(2), dimB(2)-NN+1, -1
                  do iz= iniZ, endZ
                    uu = (iz-1)*dimB(1)*dimB(2)+(iy-1)*dimB(1)+iniX
                    do ix= iniX, endX
                      tt = tt + 1
                      VXC(uu,ispin) = VXC(uu,ispin) + RBUF(tt)
                      uu = uu + 1
                    enddo
                  enddo
                enddo
              enddo
            endif

          else
            iniX = (ILbox(1,1)-idis%box(1,1,Cnode))*NSM + 1
            endX = (ILbox(2,1)-idis%box(1,1,Cnode)+1)*NSM
            iniY = (ILbox(1,2)-idis%box(1,2,Cnode))*NSM + 1
            endY = (ILbox(2,2)-idis%box(1,2,Cnode)+1)*NSM
            tt = 0
            do ispin= 1, nspin
              do iz= NN+1, 2*NN
                do iy= iniY, endY
                  uu = (iy-1)*dimB(1)+iniX
                  do ix= iniX, endX
                    tt = tt + 1
                    SBUF(tt) = BVXC(uu,iz,ispin)
                    uu = uu + 1
                  enddo
                enddo
              enddo
            enddo
            call MPI_SendRecv( SBUF, tt, MPI_grid_real, PP-1, 0,
     &                         RBUF, tt, MPI_grid_real, PP-1, 0,
     &                         MPI_Comm_world, Status, MPIerror )
            if (inter1) then
              tt = 0
              do ispin= 1, nspin
                do iz=  1, NN
                  do iy= iniY, endY
                    uu = (iz-1)*dimB(1)*dimB(2)+(iy-1)*dimB(1)+iniX
                    do ix= iniX, endX
                      tt = tt + 1
                      VXC(uu,ispin) = VXC(uu,ispin) + RBUF(tt)
                      uu = uu + 1
                    enddo
                  enddo
                enddo
              enddo
            else
              tt = 0
              do ispin= 1, nspin
                do iz= dimB(3), dimB(3)-NN+1, -1
                  do iy= iniY, endY
                    uu = (iz-1)*dimB(1)*dimB(2)+(iy-1)*dimB(1)+iniX
                    do ix= iniX, endX
                      tt = tt + 1
                      VXC(uu,ispin) = VXC(uu,ispin) + RBUF(tt)
                      uu = uu + 1
                    enddo
                  enddo
                enddo
              enddo
            endif
          endif
        endif
      enddo

      call de_alloc( RBUF, 'RBUF', 'moreMeshSubs' )
      call de_alloc( SBUF, 'SBUF', 'moreMeshSubs' )

      end subroutine gathExtMeshData


      subroutine splitwload( Nodes, Node, nm, wload, iDistr, oDistr )
!! Compute the limits of a new distribution, trying to split the load
!! of the array `wload`. We use the nested disection algorithm in
!! order to split the mesh in the 3 dimensions.
!!
!! We use the nested disection algorithm to split the load associated
!! to the vector `wload` among all the processes. The problem is that
!! every process have a different part of wload. Every time that we want
!! to split a piece of the mesh, we should find which processors have that
!! information.
!!
!! `wload` is a 3D array. In every iteration of the algorithm we should
!! decide the direction of the cut. Then we should made a reduction of
!! this 3-D array to a 1-D array (according to the selected direction).

      use mpi_siesta
      implicit none

      integer,      intent(in)      :: Nodes
        !! Total number of nodes
      integer,      intent(in)      :: Node
        !! Current process ID (from 1 to Node)
      integer,      intent(in)      :: nm(3)
        !! Number of mesh sub-divisions in each direction
      integer,      intent(in)      :: wload(*)
        !! Weights of every point of the mesh.
      type(meshDisType),intent(in)  :: iDistr
        !! Input distribution
      type(meshDisType),intent(out) :: oDistr
        !! Output distribution

!     Local variables
      character(len=*), parameter :: myName = moduName//'splitwload '
      character(len=*), parameter :: errMsg = myName//'ERROR: '
      integer                     :: PP, Lbox(2,3), Ldim,
     &                               QQ, P1, P2, POS, ini
      integer(i8b),          pointer :: lwload(:), gwload(:), recvB(:)
      logical                     :: found, inters
      integer                     :: mGdim, mLdim, nAxis, nms(3)
      integer(i8b)                   :: h1, h2
      integer,            pointer :: PROCS(:)
      integer                     :: MPIerror, Status(MPI_Status_Size)

      call timer( 'SPLOAD', 1 )

!     At the begining of the algorithm all the mesh is assigned to the
!     first node:  oDistr%box(*,*,1) = nm
      oDistr%box(1,1,1) = 1
      oDistr%box(2,1,1) = nm(1)
      oDistr%box(1,2,1) = 1
      oDistr%box(2,2,1) = nm(2)
      oDistr%box(1,3,1) = 1
      oDistr%box(2,3,1) = nm(3)
      oDistr%box(1:2,1:3,2:Nodes) = 0

      nms = nm

!     Array PROCS will contain the number of processes that are associated to
!     every box. At the begining all the mesh is assigned to process 1, then
!     PROCS(1)=Nodes, while the rest are equal to zero
      nullify( PROCS, lwload, gwload, recvB )
      call re_alloc( PROCS, 1, Nodes, 'PROCS', 'moreMeshSubs' )
      PROCS(1)       = Nodes
      PROCS(2:Nodes) = 0

      found = .true.
      do while(found)
!       Choose the direction to cut the mesh
        nAxis = 3
        if (nms(2).gt.nms(nAxis)) nAxis = 2
        if (nms(1).gt.nms(nAxis)) nAxis = 1
        nms(nAxis) = (nms(nAxis)+1)/2

!       Check if we still have to keep cutting the mesh
        found = .false.
        do PP=Nodes, 1, -1
          if (PROCS(PP).GT.1) then
!           There are more than one processes associated to the mesh
!           of process PP. We are going to split the mesh in two parts
!           of p1 and p2 processors.
            p1 = PROCS(PP)/2
            p2 = PROCS(PP) - p1
            found = .true.
!           Check if the current partition has intersection with the piece of
!           mesh that we want to cut.
            call boxIntersection( oDistr%box(:,:,PP),
     &                            iDistr%box(:,:,Node),
     &                            Lbox, inters )
            if (Node.eq.PP) then
              mGdim = oDistr%box(2,nAxis,PP)-oDistr%box(1,nAxis,PP)+1

              call re_alloc( gwload, 1, mGdim, 'gwload','moreMeshSubs' )
              call re_alloc( recvB, 1, mGdim, 'recvB', 'moreMeshSubs' )
            endif

            if (inters) then
!             If there is an intersection I should reduce the intersected part
!             from a 3-D array to a 1-D array.
              mLdim = Lbox(2,nAxis) - Lbox(1,nAxis) + 1
              call re_alloc( lwload, 1, mLdim, 'lwload',
     &                       'moreMeshSubs' )
              call reduce3Dto1D( nAxis, iDistr%box(:,:,Node), Lbox,
     &                           wload, lwload )
            endif
            if (Node.eq.PP) then
!             If, I'm the process PP I should receive the information from other
!             processes
              gwload = 0
              do QQ= 1, Nodes
                call boxIntersection( oDistr%box(:,:,PP),
     &                                iDistr%box(:,:,QQ),
     &                                Lbox, inters )
                if (inters) then
                  Ldim = Lbox(2,nAxis) - Lbox(1,nAxis) + 1
                  ini  = Lbox(1,nAxis) - oDistr%box(1,nAxis,PP)
                  if (PP.eq.QQ) then
                    gwload(ini+1:ini+Ldim) = 
     &              gwload(ini+1:ini+Ldim) + lwload(1:Ldim)
                  else
                    call mpi_recv( recvB, Ldim, MPI_INTEGER8, QQ-1, 1,
     &                             MPI_Comm_world, Status, MPIerror )
                    gwload(ini+1:ini+Ldim) =
     &              gwload(ini+1:ini+Ldim) + recvB(1:Ldim)
                  endif
                endif
              enddo
              call de_alloc( recvB, 'recvB', 'moreMeshSubs' )

!             Process PP computes where to cut the mesh
              call vecBisec( mGdim, gwload(1:mGdim),
     &                        PROCS(PP), POS, h1, h2 )
              call de_alloc(gwload, 'gwload', 'moreMeshSubs')

            else if (inters) then
!             If, I'm not the process PP I should send the information to
!             the process PP
              call MPI_Send( lwload, mLdim,
     &                       MPI_INTEGER8, PP-1, 1, MPI_Comm_World,
     &                       MPIerror )
            endif

            if (associated(lwload))
     &        call de_alloc(lwload, 'lwload', 'moreMeshSubs')

!           Process PP send the position of the cut to the rest of processes
            call MPI_Bcast( pos, 1, MPI_integer, PP-1,
     &                      MPI_Comm_World, MPIerror )

!           We have splitted the piece of mesh associated to process PP
!           in two parts. One would be stored in position PP and the other
!           would be stored in position PP+P1
            QQ                     = PP + P1
            oDistr%box(1:2,1:3,QQ) = oDistr%box(1:2,1:3,PP)
            pos                    = oDistr%box(1,naxis,QQ) + pos
            oDistr%box(1,naxis,QQ) = pos
            oDistr%box(2,naxis,PP) = pos - 1
!           We should actualize the numbers of processes associated to PP and QQ
            PROCS(PP)              = P1
            PROCS(QQ)              = P2
          endif
        enddo
      enddo

      call de_alloc( PROCS, 'PROCS', 'moreMeshSubs')
      call timer( 'SPLOAD', 2 )

      end subroutine splitwload


      subroutine reduce3Dto1D( iaxis, Ibox, Lbox, wload, lwload )
!! Given a 3-D array, `wload`, we will make a reduction of its values
!! to one of its dimensions (`iaxis`). `Ibox` gives the limits of the
!! input array `wload` and `Lbox` gives the limits of the part that we
!! want to reduce.
!!
!! First we compute the 3 dimensions of the input array and the
!! intersection. We accumulate the values of the intersection into a
!! 1-D array.
!!
!!     IF (iaxis=1) lwload(II) = SUM(wload(II,*,*))
!!     IF (iaxis=2) lwload(II) = SUM(wload(*,II,*))
!!     IF (iaxis=3) lwload(II) = SUM(wload(*,*,II))

      implicit none

      integer,    intent(in) :: iaxis
        !! Axe to be reduced
      integer,    intent(in) :: Ibox(2,3)
        !! Limits of the input array
      integer,    intent(in) :: Lbox(2,3)
        !! Limits of the intersection that we want to reduce
      integer,    intent(in) :: wload(*)
        !! 3-D array that we want to reduce to one of
        !! its dimensions
      integer(i8b), intent(out) :: lwload(*)
        !! 1-D array. Reduction of the intersected part
        !! of wload

!     Local variables
      integer              :: Idim(3), Ldim(3), ind, ind1, ind2, ind3,
     &                        I1, I2, I3

!     Dimensions of the input array
      Idim(1) = Ibox(2,1) - Ibox(1,1) + 1
      Idim(2) = Ibox(2,2) - Ibox(1,2) + 1
      Idim(3) = Ibox(2,3) - Ibox(1,3) + 1

!     Dimensions of the intersection.
      Ldim(1) = Lbox(2,1) - Lbox(1,1) + 1
      Ldim(2) = Lbox(2,2) - Lbox(1,2) + 1
      Ldim(3) = Lbox(2,3) - Lbox(1,3) + 1

      if (iaxis.eq.1) then
!       Reduction into the X-axis
        lwload(1:Ldim(1)) = 0

        ind3 = (Lbox(1,3)-Ibox(1,3))*Idim(1)*Idim(2)
        ind1 = Lbox(1,1)-Ibox(1,1)
        do I3= 1, Ldim(3)
          ind2 = (Lbox(1,2)-Ibox(1,2))*Idim(1)
          do I2= 1, Ldim(2)
            ind = ind3 + ind2 + ind1 + 1
            do I1= 1, Ldim(1)
              lwload(I1) = lwload(I1) + wload(ind)
              ind        = ind + 1
            enddo
            ind2 = ind2 + Idim(1)
          enddo
          ind3 = ind3 + Idim(1)*Idim(2)
        enddo
      else if (iaxis.eq.2) then
!       Reduction into the Y-axis
        lwload(1:Ldim(2)) = 0

        ind3 = (Lbox(1,3)-Ibox(1,3))*Idim(1)*Idim(2)
        ind1 = Lbox(1,1)-Ibox(1,1)
        do I3= 1, Ldim(3)
          ind2 = (Lbox(1,2)-Ibox(1,2))*Idim(1)
          do I2= 1, Ldim(2)
            ind = ind3 + ind2 + ind1 + 1
            do I1= 1, Ldim(1)
              lwload(I2) = lwload(I2) + wload(ind)
              ind        = ind + 1
            enddo
            ind2 = ind2 + Idim(1)
          enddo
          ind3 = ind3 + Idim(1)*Idim(2)
        enddo
      else
!       Reduction into the Z-axis
        lwload(1:Ldim(3)) = 0

        ind3 = (Lbox(1,3)-Ibox(1,3))*Idim(1)*Idim(2)
        ind1 = Lbox(1,1)-Ibox(1,1)
        do I3= 1, Ldim(3)
          ind2 = (Lbox(1,2)-Ibox(1,2))*Idim(1)
          do I2= 1, Ldim(2)
            ind = ind3 + ind2 + ind1 + 1
            do I1= 1, Ldim(1)
              lwload(I3) = lwload(I3) + wload(ind)
              ind        = ind + 1
            enddo
            ind2 = ind2 + Idim(1)
          enddo
          ind3 = ind3 + Idim(1)*Idim(2)
        enddo
      endif

      end subroutine reduce3Dto1D


      subroutine vecBisec( nval, values, nparts, pos, h1, h2 )
!! Bisection of the load associated to an array.
!!
!! We want to split array `values` in `nparts`, but in this call to
!! `vecBisec` we are going to make only one cut. First, we split `nparts`
!! in two parts: `p1=nparts/2` and `p2=nparts-p1`. Then we compute the total
!! load of the array `values` (`total`) and the desired load for the
!! first part:
!!
!!     halfG = (total*p1)/nparts
!!
!! Finally, we try to find the position inside `values` where we are
!! nearer of the the desired solution.

      implicit none

      integer,    intent(in) :: nval
        !! Dimension of the input array
      integer(i8b),  intent(in) :: values(nval)
        !! Input array
      integer,    intent(in) :: nparts
        !! Numbers of partitions that we want to make from
        !! the input array (in this call we only make one cut)

      integer,   intent(out) :: pos
        !! Position of the cut
      integer(i8b), intent(out) :: h1
        !! Load of the first part
      integer(i8b), intent(out) :: h2
        !! Load of the second part

!     Local variables
      integer               :: p1, p2, ii
      integer(i8b)             :: total, halfG, halfL

      if (nparts.gt.1) then
!       Split the number of parts in 2
        p1    = nparts/2
        p2    = nparts - p1

!       Compute the total load of the array
        total = 0
        do ii= 1, nval
          total = total + values(ii)
        enddo
!       Desired load of the first part
        halfG = (total*p1)/nparts

        halfL = 0
        pos   = 0
!       Loop until we reach the solution
        do while(halfL.lt.halfG)
          pos   = pos + 1
          if (pos.eq.nval+1) STOP 'ERROR in vecBisec'
          halfL = halfL + values(pos)
        enddo
!       Check if the previous position is better than the
!       current position
        if ((halfL-values(pos)*p2/nparts).gt.halfG) then
          halfL = halfL - values(pos)
          pos   = pos - 1
        endif
        h1 = halfL
        h2 = total-halfL
      endif

      end subroutine vecBisec


#ifdef REORD1
      subroutine reordMeshNumbering( distr1, distr2 )
!!@warning
!! This version of subroutine is called if
!! the directive `REORD1` is defined.
!!@endwarning
!!
!! Given a new distribution, `distr2`, reasign each box to the proper
!! process. We use the following criteria:
!! > Minimize the number of communications. Data don't need to
!! > be communicated if it belongs to the same process in
!! > different data distributions
!!
!! The output values are stored in the current module:
!!        `[[moreMeshSubs(module):meshDistr(variable)]](distr2)%box(:,:,:)`

      implicit none

      type(meshDisType),   intent(in) :: distr1
        !! First distribution
      type(meshDisType),  intent(out) :: distr2
        !! Second distribution

!     Local variables
      integer                         :: P1, P2, P3, Lbox(2,3), II, I1,
     &                                   PermI
      integer,                pointer :: Isiz(:,:), perm(:), weig(:),
     &                                   invp(:), box(:,:,:)=>null()
      logical                         :: inters

!     Allocate local arrays
      nullify( Isiz, perm, invp, weig )
      call re_alloc( Isiz, 1, Nodes, 1, Nodes, 'Isiz', 'moreMeshSubs' )
      call re_alloc( perm, 1, Nodes, 'perm', 'moreMeshSubs' )
      call re_alloc( invp, 1, Nodes, 'invp', 'moreMeshSubs' )
      call re_alloc( weig, 1, Nodes, 'weig', 'moreMeshSubs' )

!     Check the intersections sizes between the two distributions
      Isiz(1:nodes,1:nodes) = 0
      do P1= 1, Nodes
        II=-1
        do P2= 1, Nodes
          call boxIntersection( distr1%box(:,:,P1),
     &                          distr2%box(:,:,P2), Lbox, inters )
          if (inters) then
            Isiz(P2,P1) = (Lbox(2,1)-Lbox(1,1)+1)*
     &                    (Lbox(2,2)-Lbox(1,2)+1)*
     &                    (Lbox(2,3)-Lbox(1,3)+1)
            if (Isiz(P2,P1).gt.II) then
              II = Isiz(P2,P1)
              I1 = P2
            endif
          endif
        enddo
        weig(P1) = II
        perm(P1) = I1
      enddo

!     Choose a proper permutation for every row
      invp(1:Nodes) = 0
      do P1= 1, Nodes
        II = -1
!       Choose the node with higher weight among those not permuted before
        do P2= 1, Nodes
          if (perm(P2).gt.0) then
            if (weig(P2).gt.II) then
              II = weig(P2)
              I1 = P2
            endif
          endif
        enddo

!       Save the permutation for this node (a negative number means that
!       the node has been permuted.
        PermI       = perm(I1)
        invp(PermI) = I1
        perm(I1)    = -PermI

!       Change the permutation of those nodes who pretend to use the
!       permutation permI
        do P2= 1, Nodes
          if (perm(P2).eq.PermI) then
            II= -1
            do P3= 1, Nodes
              if (invp(P3).eq.0 .and. Isiz(P3,P2).gt.II) then
                II = Isiz(P3,P2)
                I1 = P3
              endif
            enddo
            weig(P2) = II
            perm(P2) = I1
          endif
        enddo
      enddo

      call re_alloc( box, 1, 2, 1, 3, 1, Nodes, 'box', 'moreMeshSubs' )
      box(1:2,1:3,1:Nodes) = distr2%box(1:2,1:3,1:Nodes)
      do P1= 1, Nodes
        II = -perm(P1)
        distr2%box(1:2,1:3,P1) = box(1:2,1:3,II)
      enddo

      call de_alloc( box, 'box', 'moreMeshSubs' )
      call de_alloc( weig, 'weig', 'moreMeshSubs' )
      call de_alloc( invp, 'invp', 'moreMeshSubs' )
      call de_alloc( perm, 'perm', 'moreMeshSubs' )
      call de_alloc( Isiz, 'Isiz', 'moreMeshSubs' )
      end subroutine reordMeshNumbering


#else
      subroutine reordMeshNumbering( distr1, distr2 )
!!@warning
!! This version of subroutine is called if
!! the directive `REORD1` is **NOT** defined.
!!
!! Here an integer parameter `PROCS_PER_NODE` is also
!! used as an input. Value of `PROCS_PER_NODE` is either
!! read from .fdf-file or set to 4 as default.
!!@endwarning
!!
!! Given a new distribution, `distr2`, reasign each box to the proper
!! process. We use the following criteria:
!! > * Minimize the number of communications. Data don't need to
!! >   be communicated if it belongs to the same process in
!! >   different data distributions
!! > * Distribute memory needs among different NODES (group of processes
!! >   that shares the same memory)
!!
!! The output values are stored in the current module:
!!        `[[moreMeshSubs(module):meshDistr(variable)]](distr2)%box(:,:,:)`
!!
!!#### Behavior
!!
!! 1. Compute the size of all the boxes of the second distribution
!! 2. Reorder the list of boxes according to its size
!! 3. Create a list of buckets

      use fdf
      implicit none

      type(meshDisType),   intent(in) :: distr1
        !! First distribution
      type(meshDisType),  intent(out) :: distr2
        !! Second distribution

!     Local variables
      integer                         :: P1, P2, I1, I2, J1, J2, J3,
     &                                   K1, K2, K3, NN, NB, NM, SI,
     &                                   SIMAX, Lbox(2,3)
      integer,                pointer :: Nsiz(:), perm(:), Gprm(:),
     &                                   chkb(:), box1(:,:), box2(:,:),
     &                                   box(:,:,:)=>null()
      integer                         :: PROCS_PER_NODE
      logical                         :: inters

      PROCS_PER_NODE = fdf_get( 'PROCS_PER_NODE', 4 )

!     Create groups of PROCS_PER_NODE processes
      NN = nodes+PROCS_PER_NODE-1
      NB = NN/PROCS_PER_NODE           ! Number of buckets
      NM = MOD(NN,PROCS_PER_NODE)+1    ! Size of the last bucket

!     Allocate local arrays
      nullify( Nsiz, perm, Gprm, chkb )
      call re_alloc( Nsiz, 1, Nodes, 'Nsiz', 'moreMeshSubs' )
      call re_alloc( perm, 1, Nodes, 'perm', 'moreMeshSubs' )
      call re_alloc( Gprm, 1, Nodes, 'Gprm', 'moreMeshSubs' )
      call re_alloc( chkb, 1, NB,    'chkb', 'moreMeshSubs' )
      call re_alloc( box, 1, 2, 1, 3, 1, Nodes, 'box', 'moreMeshSubs' )

!     Compute the size of all the boxes of the second distribution
      do P1= 1, Nodes
        box2 => distr2%box(:,:,P1)
        Nsiz(P1) = (box2(2,1)-box2(1,1)+1)*
     &             (box2(2,2)-box2(1,2)+1)*
     &             (box2(2,3)-box2(1,3)+1)
        perm(P1) = P1
      enddo

!     Reorder the list of boxes according to its size
      call myQsort( Nodes, Nsiz, perm )

      Gprm(1:Nodes) = 0
      P1=0

!     We distribute processes in "buckets" of size PROCS_PER_NODE
!     We have a total number of NB "buckets".
      DO I1= 1, PROCS_PER_NODE
!       At every step of loop I1, we assign a box to every bucket
        if (I1.EQ.NM+1) NB = NB - 1

!       Reset chkb. All buckets are empty.
        chkb(1:NB) = 0

        DO I2= 1, NB
!         At every step of loop I2, we assign a box to a different bucket
          P1 = P1 + 1
          P2 = perm(P1)  ! P2 is the "P1"th biggest box

          box2 => distr2%box(:,:,P2)
          J2 = 1
          J3 = 1
          SIMAX = -1
          DO J1=1, Nodes
!           J1=node; J2=position inside the bucket; J3=bucket index
            if (chkb(J3).eq.0 .and. Gprm(J1).eq.0) then
!             chkb(J3).eq.0 => The current bucket is not in use
!             Gprm(J1).eq.0 => The node has not been permuted yet
              box1 => distr1%box(:,:,J1)
              call boxIntersection( box1, box2, Lbox, inters )
              if (inters) then
                SI = (Lbox(2,1)-Lbox(1,1)+1)*
     &               (Lbox(2,2)-Lbox(1,2)+1)*
     &               (Lbox(2,3)-Lbox(1,3)+1)
              else
                SI = 0
              endif
              if (SI.gt.SIMAX) then
!               Save the information of the current node if it has the
!               biggest intersection with box P2
                SIMAX = SI
                K1 = J1
                K3 = J3
              endif
            endif
!           Update information about bucket index and position
            J2 = J2 + 1
            if (J2.gt.PROCS_PER_NODE) then
              J2 = 1
              J3 = J3 + 1
            endif
          ENDDO

!         box(P2) will be set to process K1 (that belongs to bucket K3)
          chkb(K3) = 1
          Gprm(K1) = P2
        ENDDO
      ENDDO

!     Reorder boxes of the second distribution according to Gprm
      box(1:2,1:3,1:Nodes) = distr2%box(1:2,1:3,1:Nodes)
      do P1= 1, Nodes
        P2 = Gprm(P1)
        distr2%box(1:2,1:3,P1) = box(1:2,1:3,P2)
      enddo

!     Deallocate local arrays
      call de_alloc( box,  'box',  'moreMeshSubs' )
      call de_alloc( chkb, 'chkb', 'moreMeshSubs' )
      call de_alloc( Gprm, 'Gprm', 'moreMeshSubs' )
      call de_alloc( perm, 'perm', 'moreMeshSubs' )
      call de_alloc( Nsiz, 'Nsiz', 'moreMeshSubs' )

      end subroutine reordMeshNumbering
#endif


      subroutine compMeshComm( distr1, distr2, mcomm )
!! Find the communications needed to transform one array that uses
!! distribution `distr1` to distribution `distr2`
!!
!! Count the number of intersections between the source distribution
!! and the destiny distribution. Every intersection represents a
!! communication. Then we call [[scheduleComm(proc)]]
!! to optimize the order of these
!! communications. Finally, we save the communications that belongs to
!! the current process in the variable `mcomm`

      use scheComm
      implicit none

      type(meshDisType),   intent(in) :: distr1
        !! Source distribution
      type(meshDisType),   intent(in) :: distr2
        !! Destination distribution

      type(meshCommType), intent(out) :: mcomm
        !! Communications needed

!     Local variables
      integer                         :: P1, P2, ncom, Gcom, Lcom,
     &                                   Lind, Lbox(2,3)
      integer,                pointer :: src(:), dst(:)
      logical                         :: inters
      type(COMM_T)                    :: comm

!     count the number of intersections between Source distribution and
!     destiny distribution. Every intersection represents a communication.
      ncom = 0
      do P1= 1, Nodes
        do P2= 1, Nodes
          call boxIntersection( distr1%box(:,:,P1),
     &                          distr2%box(:,:,P2), Lbox, inters )
          if (inters) ncom = ncom + 1
        enddo
      enddo
      Gcom = ncom

!     Allocate local arrays
      nullify( src, dst )
      call re_alloc( src, 1, Gcom, 'src', 'moreMeshSubs' )
      call re_alloc( dst, 1, Gcom, 'dst', 'moreMeshSubs' )

!     Make a list of communications
      ncom = 0
      do P1= 1, Nodes
        do P2= 1, Nodes
          call boxIntersection( distr1%box(:,:,P1),
     &                          distr2%box(:,:,P2), Lbox, inters )
          if (inters) then
            ncom      = ncom + 1
            src(ncom) = P1
            dst(ncom) = P2
          endif
        enddo
      enddo

      comm%np = Nodes
!     reschedule the communications in order to minimize the time
      call scheduleComm( Gcom, src, dst, comm )

!     Count the number of communications of the current process
      ncom = 0
      do P1= 1, comm%ncol
        if (comm%ind(P1,Node+1).ne.0) ncom = ncom + 1
      enddo
      Lcom = ncom

!     Allocate memory to store data of the communications of the
!     current process.
      nullify( mcomm%src, mcomm%dst )
      call re_alloc( mcomm%src, 1, Lcom, 'mcomm%src', 'moreMeshSubs' )
      call re_alloc( mcomm%dst, 1, Lcom, 'mcomm%dst', 'moreMeshSubs' )

!     Save the list of communications for the current process
      ncom = 0
      do P1= 1, comm%ncol
        Lind = comm%ind(P1,Node+1)
        if (Lind.ne.0) then
          ncom            = ncom + 1
          mcomm%src(ncom) = src(Lind)
          mcomm%dst(ncom) = dst(Lind)
        endif
      enddo
      mcomm%ncom = ncom

      call de_alloc( comm%ind, 'comm%ind', 'scheComm' )
      call de_alloc( dst,      'dst',      'moreMeshSubs' )
      call de_alloc( src,      'src',      'moreMeshSubs' )

      end subroutine compMeshComm
#endif
      END MODULE moreMeshSubs
