! ---
! Copyright (C) 1996-2016	The SIESTA group
!  This file is distributed under the terms of the
!  GNU General Public License: see COPYING in the top directory
!  or http://www.gnu.org/copyleft/gpl.txt .
! See Docs/Contributors.txt for a list of contributors.
! ---

      module parallel
!! Parallelisation related global parameters

      implicit none

      integer, save :: SIESTA_group, SIESTA_comm
      logical, save :: SIESTA_worker = .false.
      integer, save :: Node = 0
      integer, save :: Nodes = 1
      integer, save :: PEXSINodes = 1
      integer, save :: BlockSize  = 24
        !! This is the blocking factor used to divide up
        !! the arrays over the processes for the Scalapack
        !! routines. Setting this value is a compromise
        !! between dividing up the orbitals over the processors
        !! evenly to achieve load balancing and making the
        !! local work efficient. Typically a value of about
        !! 10 is good, but optimisation may be worthwhile.
        !! A value of 1 is very bad for any number of processors
        !! and a large value may also be less than ideal.
      integer, save :: ProcessorY = 1
        !! Second dimension of processor grid in mesh point
        !! parallelisation - note that the first dimension
        !! is determined by the total number of processors
        !! in the current job. Also note that this number
        !! must be a factor of the total number of processors.
        !! Furthermore on many parallel machines (e.g. T3E)
        !! this number must also be a power of 2.

      logical, save :: IOnode = .true.

      public

      interface operator(.PARCOUNT.)
        module procedure parcount
      end interface

      private :: parcount

      contains

      subroutine parallel_init()
        !! Initializes Node, Nodes, and IOnode

#ifdef MPI
      use mpi_siesta, only: MPI_Comm_World
      logical, save:: initialized=.false.
      integer :: MPIerror
      if (.not.initialized) then
        call MPI_Comm_Rank( MPI_Comm_World, Node, MPIerror )
        call MPI_Comm_Size( MPI_Comm_World, Nodes, MPIerror )
        IOnode = (Node==0)
        initialized = .true.
      end if
#endif
      end subroutine parallel_init

      elemental function parcount(Nodes,N)
        !! Convert a (positive) counter into a counter divisable by
        !! the number of Nodes.
        !!
        !! It works by this:  
        !! `PN = Nodes .PARCOUNT. N`
        !!
        !! We make it elemental for obvious reasons

      integer, intent(in) :: Nodes, N
      integer :: parcount

      if ( mod(N,Nodes) == 0 ) then
         parcount = N
      else
         parcount = N + Nodes - mod(N,Nodes)
      end if

      end function parcount

      end module parallel
