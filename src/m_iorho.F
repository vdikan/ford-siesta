! 
! Copyright (C) 1996-2016	The SIESTA group
!  This file is distributed under the terms of the
!  GNU General Public License: see COPYING in the top directory
!  or http://www.gnu.org/copyleft/gpl.txt.
! See Docs/Contributors.txt for a list of contributors.
!
      module m_iorho
!! To have both reading and writing, allowing a change in the
!! values of variables used as array dimensions, is not very
!! safe. Thus, the old iorho has been split in three routines:
!!
!! 1. [[write_rho(proc)]]: Writes grid magnitudes to file
!! 2. [[read_rho(proc)]]: Reads grid magnitudes from file
!! 3. [[check_rho(proc)]]: Checks the appropriate dimensions for reading.
!!
!! The magnitudes are saved in `SINGLE PRECISION` (sp), regardless
!! of the internal precision used in the program. Historically,
!! the internal precision has been "single".

      use precision,    only : dp, grid_p, sp
      use parallel,     only : Node, ProcessorY
      use sys,          only : die
      use alloc,        only : re_alloc, de_alloc
#ifdef MPI
      use mpi_siesta
      use parallel,     only : Nodes
      use parallelsubs, only : HowManyMeshPerNode
#endif

      implicit          none

      character(len=*), parameter  :: fform = "unformatted"

      public :: write_rho, read_rho, check_rho

      private

      CONTAINS

      subroutine write_rho( fname, cell, mesh, nsm, maxp, nspin, rho)
      !! author: J.Soler
      !! date: July 1997
      !!
      !! Writes the electron density or potential at the mesh points.
      !!
      !! Parallel modifications added, while maintaining independence
      !! of density matrix on disk from parallel distribution. Uses a
      !! block distribution for density matrix. It is important to
      !! remember that the density matrix is divided so that all
      !! sub-points reside on the same node.
      !!
      !! Modified by J.D.Gale March 1999.
      !!
!!@note
!! In order to achieve a consistent format of the disk file
!! each record in the unformatted file corresponds to one pair of
!! Y and Z values. Hence there will be a total of mesh(2) x mesh(3)
!! records.
!!@endnote

      character(len=*), intent(in) ::     fname
        !! File name
      integer, intent(in)          ::     nsm
        !! Number of sub-mesh points per mesh point  
        !! (not used in this version)
      integer, intent(in)          ::     mesh(3)
        !! Number of mesh divisions of each lattice vector
      integer, intent(in)          ::     maxp
      integer, intent(in)          ::     nspin
      real(grid_p), intent(in)     ::     rho(maxp,nspin)
      real(dp), intent(in)         ::     cell(3,3)

      external          io_assign, io_close, memory

      ! Internal variables and arrays
      integer    i, ip, iu, is, j, np,
     .           BlockSizeY, BlockSizeZ, ProcessorZ,
     .           meshnsm(3), NRemY, NRemZ,
     .           iy, iz, izm, Ind, ir

#ifdef MPI
      integer    Ind2, MPIerror, Request,
     .           Status(MPI_Status_Size), BNode, NBlock
      real(grid_p), pointer :: bdens(:) => null()
#endif
      real(sp),     pointer :: temp(:) => null()

#ifdef MPI
#ifdef DEBUG
        call write_debug( 'ERROR: write_rho not ready yet' )
        call write_debug( fname )
#endif

! Work out density block dimensions
      if (mod(Nodes,ProcessorY).gt.0)
     $     call die('ERROR: ProcessorY must be a factor of the' //
     $     ' number of processors!')
      ProcessorZ = Nodes/ProcessorY
      BlockSizeY = ((((mesh(2)/nsm)-1)/ProcessorY) + 1)*nsm
      call re_alloc(bdens, 1, BlockSizeY*mesh(1), 'bdens', 'write_rho')
#else
      ProcessorZ = 1
#endif

      call re_alloc( temp, 1, mesh(1), 'temp', 'write_rho' )
! Open file
      if (Node.eq.0) then
        call io_assign( iu )
        open( iu, file=fname, form=fform, status='unknown' )
      endif

      np = mesh(1) * mesh(2) * mesh(3)

      meshnsm(1) = mesh(1)/nsm
      meshnsm(2) = mesh(2)/nsm
      meshnsm(3) = mesh(3)/nsm

! Write data
      if (Node.eq.0) then
        if (fform .eq. 'formatted') then
          do i = 1,3
            write(iu,*) (cell(j,i),j=1,3)
          enddo
          write(iu,*) mesh, nspin
        else
          write(iu) cell
          write(iu) mesh, nspin
        endif
      endif

! Outer loop over spins
        do is = 1,nspin

          Ind = 0

! Loop over Z dimension of processor grid
          do iz = 1,ProcessorZ

            BlockSizeZ = (meshnsm(3)/ProcessorZ)
            NRemZ = meshnsm(3) - BlockSizeZ*ProcessorZ
            if (iz-1.lt.NRemZ) BlockSizeZ = BlockSizeZ + 1
            BlockSizeZ = BlockSizeZ*nsm

! Loop over local Z mesh points
            do izm = 1,BlockSizeZ

! Loop over blocks in Y mesh direction
              do iy = 1,ProcessorY

! Work out size of density sub-matrix to be transfered
                BlockSizeY = (meshnsm(2)/ProcessorY)
                NRemY = meshnsm(2) - BlockSizeY*ProcessorY
                if (iy-1.lt.NRemY) BlockSizeY = BlockSizeY + 1
                BlockSizeY = BlockSizeY*nsm

#ifdef MPI
                NBlock = BlockSizeY*mesh(1)
! Work out which node block is stored on
                BNode = (iy-1)*ProcessorZ + iz - 1

                if (BNode.eq.0.and.Node.eq.BNode) then
#endif
! If density sub-matrix is local Node 0 then just write it out
                  if (fform .eq. 'formatted') then
                    do ir = 1,BlockSizeY
                      write(iu,'(e15.6)') (rho(Ind+ip,is),
     .                  ip=1,mesh(1))
                      Ind = Ind + mesh(1)
                    enddo
                  else
!AG**
                    do ir = 1,BlockSizeY
                      temp(1:mesh(1)) =
     $                      real(rho(Ind+1:Ind+mesh(1),is),kind=sp)
                      write(iu) temp(1:mesh(1))
                      Ind = Ind + mesh(1)
                    enddo
                  endif

#ifdef MPI
                elseif (Node.eq.0) then
! If this is Node 0 then recv and write density sub-matrix
                  call MPI_IRecv(bdens,NBlock,MPI_grid_real,BNode,1,
     .              MPI_Comm_World,Request,MPIerror)
                  call MPI_Wait(Request,Status,MPIerror)

                elseif (Node.eq.BNode) then
! If this is the Node where the density sub-matrix is, then send
                  call MPI_ISend(rho(Ind+1,is),NBlock,MPI_grid_real,0,1,
     .              MPI_Comm_World,Request,MPIerror)
                  call MPI_Wait(Request,Status,MPIerror)
                  Ind = Ind + NBlock

                endif

                if (BNode.ne.0) then
                  call MPI_Barrier(MPI_Comm_World,MPIerror)
                  if (Node.eq.0) then
                    Ind2 = 0
                    if (fform .eq. 'formatted') then
                      do ir = 1,BlockSizeY
                        write(iu,'(e15.6)') (bdens(Ind2+ip),ip=1,
     .                    mesh(1))
                        Ind2 = Ind2 + mesh(1)
                      enddo
                    else
!AG**
                      do ir = 1,BlockSizeY
                      temp(1:mesh(1)) =
     $                      real(bdens(Ind2+1:Ind2+mesh(1)),kind=sp)
                        write(iu) temp(1:mesh(1))
                        Ind2 = Ind2 + mesh(1)
                      enddo

                    endif
                  endif
                endif
#endif

              enddo

            enddo

          enddo

        enddo

        if (Node.eq.0) then
! Close file
          call io_close( iu )
        endif

#ifdef MPI
C Deallocate density buffer memory
      call de_alloc( bdens, 'bdens', 'write_rho' )
#endif
      call de_alloc( temp, 'temp', 'write_rho' )
      end subroutine write_rho


      subroutine read_rho( fname, cell, mesh, nsm, maxp, nspin, rho)
      !! author: J.Soler
      !! date: July 1997
      !!
      !! Reads the electron density or potential at the mesh points.
      !!
      !! If the values of maxp or nspin on input are less than
      !! those required to copy the array rho from the file, the subroutine
      !! stops. Use subroutine [[check_rho(proc)]]
      !! to find the right `maxp` and `nspin`.
      !!
!!@note
!! AG: The magnitudes are saved in SINGLE PRECISION (sp), regardless
!!     of the internal precision used in the program. Historically,
!!     the internal precision has been "single".
!!@endnote
      !!
      !! Parallel modifications added, while maintaining independence
      !! of density matrix on disk from parallel distribution. Uses a
      !! block distribution for density matrix. It is important to
      !! remember that the density matrix is divided so that all
      !! sub-points reside on the same node.
      !!
      !! Modified by J.D.Gale March 1999.
      !!
!!@note
!! In order to achieve a consistent format of the disk file
!! each record in the unformatted file corresponds to one pair of
!! Y and Z values. Hence there will be a total of mesh(2) x mesh(3)
!! records.
!!@endnote

      character(len=*), intent(in) ::     fname
        !! File name for input or output
      integer, intent(in)          ::     nsm
        !! Number of sub-mesh points per mesh point  
        !! (not used in this version)
      integer, intent(in)          ::     maxp
        !! First dimension of array rho
      integer, intent(in)          ::     nspin
        !! Second dimension of array rho
      integer, intent(out)         ::     mesh(3)
        !! Number of mesh divisions of each lattice vector
      real(grid_p), intent(out)    ::     rho(maxp,nspin)
        !! Electron density
      real(dp), intent(out)        ::     cell(3,3)
        !! Lattice vectors

      external          io_assign, io_close, memory

! Internal variables and arrays
      integer    ip, iu, is, np, ns,
     .           BlockSizeY, BlockSizeZ, ProcessorZ,
     .           meshnsm(3), npl, NRemY, NRemZ,
     .           iy, iz, izm, Ind, ir

#ifdef MPI
      integer    Ind2, MPIerror, Request, meshl(3),
     .           Status(MPI_Status_Size), BNode, NBlock
      logical    ltmp
      real(grid_p), pointer :: bdens(:) => null()
#endif
      real(sp),     pointer :: temp(:) => null()

      logical    baddim, found


#ifdef MPI
! Work out density block dimensions
      if (mod(Nodes,ProcessorY).gt.0)
     $     call die('ERROR: ProcessorY must be a factor of the' //
     $     ' number of processors!')
      ProcessorZ = Nodes/ProcessorY
      BlockSizeY = ((((mesh(2)/nsm)-1)/ProcessorY) + 1)*nsm
      call re_alloc( bdens, 1, BlockSizeY*mesh(1), 'bdens', 'read_rho' )
#else
      ProcessorZ = 1
#endif

! Check if input file exists
        if (Node.eq.0) then
          inquire( file=fname, exist=found )
          if (.not. found) call die("Cannot find file " // trim(fname))
        endif

! Open file
          if (Node.eq.0) then
            call io_assign( iu )
            open( iu, file=fname, form=fform, status='old' )

! Read cell vectors and number of points
            if (fform .eq. 'formatted') then
              read(iu,*) cell
              read(iu,*) mesh, ns
            else
              read(iu) cell
              read(iu) mesh, ns
            endif
          endif
          call re_alloc( temp, 1, mesh(1), 'temp', 'read_rho' )
#ifdef MPI
          call MPI_Bcast(cell(1,1),9,MPI_double_precision,0,
     .      MPI_Comm_World,MPIerror)
          call MPI_Bcast(mesh,3,MPI_integer,0,MPI_Comm_World,MPIerror)
          call MPI_Bcast(ns,1,MPI_integer,0,MPI_Comm_World,MPIerror)
#endif
          np = mesh(1) * mesh(2) * mesh(3)

! Get local dimensions
          meshnsm(1) = mesh(1)/nsm
          meshnsm(2) = mesh(2)/nsm
          meshnsm(3) = mesh(3)/nsm
#ifdef MPI
          call HowManyMeshPerNode(meshnsm,Node,Nodes,npl,meshl)
#else
          npl = np
#endif

! Check dimensions
          baddim = .false.
          if (ns .gt. nspin) baddim = .true.
          if (npl .gt. maxp) baddim = .true.

#ifdef MPI
! Globalise dimension check
          call MPI_AllReduce(baddim,ltmp,1,MPI_logical,MPI_Lor,
     .      MPI_Comm_World,MPIerror)
          baddim = ltmp
#endif

          if (baddim) then
             call die("Dimensions of array rho too small in read_rho")
          endif

! Outer loop over spins
          do is = 1,ns

          Ind = 0

! Loop over Z mesh direction
          do iz = 1,ProcessorZ

! Work out number of mesh points in Z direction
            BlockSizeZ = (meshnsm(3)/ProcessorZ)
            NRemZ = meshnsm(3) - BlockSizeZ*ProcessorZ
            if (iz-1.lt.NRemZ) BlockSizeZ = BlockSizeZ + 1
            BlockSizeZ = BlockSizeZ*nsm

! Loop over local Z mesh points
            do izm = 1,BlockSizeZ

! Loop over blocks in Y mesh direction
              do iy = 1,ProcessorY

! Work out size of density sub-matrix to be transfered
                BlockSizeY = (meshnsm(2)/ProcessorY)
                NRemY = meshnsm(2) - BlockSizeY*ProcessorY
                if (iy-1.lt.NRemY) BlockSizeY = BlockSizeY + 1
                BlockSizeY = BlockSizeY*nsm

#ifdef MPI
                NBlock = BlockSizeY*mesh(1)
! Work out which node block is stored on
                BNode = (iy-1)*ProcessorZ + iz - 1

                if (BNode.eq.0.and.Node.eq.BNode) then
#endif
! If density sub-matrix is local Node 0 then just read it in
                  if (fform .eq. 'formatted') then
                    do ir = 1,BlockSizeY
                      read(iu,*) (rho(Ind+ip,is),ip=1,mesh(1))
                      Ind = Ind + mesh(1)
                    enddo
                  else
!
! AG**: Check sizes of records here -- grid-precision issues
!       Either agree on a "single" format for file storage,
!       or put in some intelligence in all post-processors

                    do ir = 1,BlockSizeY
                      read(iu) temp(1:mesh(1))
                      rho(Ind+1:Ind+mesh(1),is) =
     $                              real(temp(1:mesh(1)), kind=grid_p)
                      Ind = Ind + mesh(1)
                    enddo
                  endif

#ifdef MPI
                elseif (Node.eq.0) then
C If this is Node 0 then read and send density sub-matrix
                  Ind2 = 0
                  if (fform .eq. 'formatted') then
                    do ir = 1,BlockSizeY
                      read(iu,*) (bdens(Ind2+ip),ip=1,mesh(1))
                      Ind2 = Ind2 + mesh(1)
                    enddo
                  else
!AG**
                    do ir = 1,BlockSizeY
                      read(iu) temp(1:mesh(1))
                      bdens(Ind2+1:Ind2+mesh(1)) =
     $                              real(temp(1:mesh(1)), kind=grid_p)
                      Ind2 = Ind2 + mesh(1)
                    enddo
                  endif
                  call MPI_ISend(bdens,NBlock,MPI_grid_real,BNode,1,
     .              MPI_Comm_World,Request,MPIerror)
                  call MPI_Wait(Request,Status,MPIerror)

                elseif (Node.eq.BNode) then
! If this is the Node where the density sub-matrix is, then receive
                  call MPI_IRecv(rho(Ind+1,is),NBlock,MPI_grid_real,
     .              0,1,MPI_Comm_World,Request,MPIerror)
                  call MPI_Wait(Request,Status,MPIerror)
                  Ind = Ind + NBlock

                endif

                if (BNode.ne.0) then
                  call MPI_Barrier(MPI_Comm_World,MPIerror)
                endif
#endif

              enddo

            enddo

          enddo

          enddo

! Close file
        if (Node.eq.0) then
          call io_close( iu )
        endif

#ifdef MPI
! Deallocate density buffer memory
      call de_alloc( bdens, 'bdens', 'read_rho' )
#endif
      call de_alloc( temp, 'temp', 'read_rho' )
      end subroutine read_rho


      subroutine check_rho(fname,maxp,nspin,nsm,found,overflow)

      character(len=*), intent(in) ::     fname
      integer, intent(in)          ::     nsm
        !! Number of sub-mesh points per mesh point  
        !! (not used in this version)
      integer, intent(inout)       ::     maxp
        !! Required first dimension of array rho,  
        !! equal to `mesh(1)*mesh(2)*mesh(3)`
      integer, intent(inout)       ::     nspin
        !! Number of spin polarizations (1 or 2)
      logical, intent(out)         ::     found
        !! Were data found?
      logical, intent(out)         ::     overflow
        !! True if `maxp` or `nspin` were changed

      real(dp) :: cell(3,3)
      integer  :: mesh(3)
      integer  :: meshnsm(3), npl, ns, iu, np, npmax
#ifdef MPI
      integer  :: meshl(3)
      logical  :: ltmp
      integer  :: MPIerror
#endif

! Check if input file exists
        if (Node.eq.0) then
          inquire( file=fname, exist=found )
        endif
#ifdef MPI
        call MPI_Bcast(found,1,MPI_logical,0,MPI_Comm_World,MPIerror)
#endif
        if (.not. found) return

! Open file
          if (Node.eq.0) then
            call io_assign( iu )
            open( iu, file=fname, form=fform, status='old' )

! Read cell vectors and number of points
            if (fform .eq. 'formatted') then
              read(iu,*) cell
              read(iu,*) mesh, ns
            else
              read(iu) cell
              read(iu) mesh, ns
            endif
          endif
#ifdef MPI
          call MPI_Bcast(cell(1,1),9,MPI_double_precision,0,
     .      MPI_Comm_World,MPIerror)
          call MPI_Bcast(mesh,3,MPI_integer,0,MPI_Comm_World,MPIerror)
          call MPI_Bcast(ns,1,MPI_integer,0,MPI_Comm_World,MPIerror)
#endif
          np = mesh(1) * mesh(2) * mesh(3)

! Get local dimensions
          meshnsm(1) = mesh(1)/nsm
          meshnsm(2) = mesh(2)/nsm
          meshnsm(3) = mesh(3)/nsm
#ifdef MPI
          call HowManyMeshPerNode(meshnsm,Node,Nodes,npl,meshl)
#else
          npl = np
#endif

! Check dimensions
          overflow = .false.
          if (ns .gt. nspin) overflow = .true.
          if (npl .gt. maxp) overflow = .true.

#ifdef MPI
! Globalise dimension check
          call MPI_AllReduce(overflow,ltmp,1,MPI_logical,MPI_Lor,
     .      MPI_Comm_World,MPIerror)
          overflow = ltmp
#endif

          if (overflow) then    ! Some processor has npl > maxp ...
#ifdef MPI
! Find largest value of npl
            call MPI_AllReduce(npl,npmax,1,MPI_integer,MPI_Max,
     .        MPI_Comm_World,MPIerror)
#else
            npmax = np
#endif
            maxp = npmax
            nspin = ns
          endif

          if (Node.eq.0) call io_close( iu )

          end subroutine check_rho

      end module m_iorho
