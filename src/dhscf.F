!
! Copyright (C) 1996-2016	The SIESTA group
!  This file is distributed under the terms of the
!  GNU General Public License: see COPYING in the top directory
!  or http://www.gnu.org/copyleft/gpl.txt.
! See Docs/Contributors.txt for a list of contributors.
!

!! The old dhscf has been split in two parts: an initialization routine
!! `[[dhscf_init(proc)]]`, which is called after
!! every geometry change but before
!! the main scf loop, and a `[[dhscf(proc)]]` proper,
!! which is called at every step
!! of the scf loop

!!@note
!! The mesh initialization part is now done *unconditionally*
!! in `dhscf_init`, i.e, after *every* geometry change, even if the
!! change does not involve a cell change. The reason is to avoid
!! complexity, since now the mesh parallel distributions will depend on
!! the detailed atomic positions even if the cell does not change.
!!@endnote

!! Besides, the relative cost of a "mesh only" initialization is negligible.
!! The only real observable effect would be a printout of "initmesh" data
!! at every geometry iteration.

      module m_dhscf
!! To facilitate the communication among
!! `[[dhscf_init(proc)]]` and `[[dhscf(proc)]]`,
!! some arrays that hold data which do not change during the SCF loop
!! have been made into module variables
!!
!! Some others are scratch, such as `nmpl`, `ntpl`, etc

      use precision,      only : dp, grid_p
      use m_dfscf,        only : dfscf
      implicit none

      real(grid_p),   pointer :: rhopcc(:), rhoatm(:), Vna(:)
      real(dp)                :: Uharrs     !! Harris energy
      logical                 :: IsDiag, spiral
      character(len=10)       :: shape
      integer                 :: nml(3), ntml(3), npcc,
     &                           nmpl, ntpl
      real(dp)                :: bcell(3,3), cell(3,3),
     &                           dvol, field(3), rmax, scell(3,3)
      real(dp)                :: G2mesh =  0.0_dp
      logical :: debug_dhscf = .false.
      character(len=*), parameter :: debug_fmt =
     &     '('' -- dhscf : Node '',i3,tr1,a,4(tr1,e20.7))'

      public :: dhscf_init, dhscf

      CONTAINS

      subroutine dhscf_init(nspin, norb, iaorb, iphorb,
     &                      nuo, nuotot, nua, na,
     &                      isa, xa, indxua, ucell,
     &                      mscell, G2max, ntm,
     &                      maxnd, numd, listdptr, listd, datm,
     &                      Fal, stressl)

      use precision,      only : dp, grid_p
      use parallel,       only : Node, Nodes
      use atmfuncs,       only : rcut, rcore
      use fdf
      use sys,            only : die
      use mesh,           only : xdsp, nsm, nsp, meshLim
      use parsing
#ifndef BSC_CELLXC
      use siestaXC, only : getXC        ! Returns the XC functional used
#else /* BSC_CELLXC */
      use bsc_xcmod,          only : nXCfunc, XCauth
#endif /* BSC_CELLXC */
      use alloc,          only : re_alloc, de_alloc
      use siesta_options, only : harrisfun
      use meshsubs,       only : PartialCoreOnMesh
      use meshsubs,       only : NeutralAtomOnMesh
      use meshsubs,       only : PhiOnMesh
      use meshsubs,       only : InitMesh
      use meshsubs,       only : InitAtomMesh
      use meshsubs,       only : setupExtMesh
      use meshsubs,       only : distriPhiOnMesh

      use moreMeshSubs,   only : setMeshDistr, distMeshData
      use moreMeshSubs,   only : UNIFORM, QUADRATIC, LINEAR
      use moreMeshSubs,   only : TO_SEQUENTIAL, TO_CLUSTER, KEEP

      use meshdscf,       only : createLocalDscfPointers

      use iogrid_netcdf, only: set_box_limits
#ifdef NCDF_4
      use m_ncdf_io, only : cdf_init_mesh
#endif
#ifdef BSC_CELLXC
      use cellxc_mod,     only : setGGA
#endif /* BSC_CELLXC */
      use m_efield,       only : initialize_efield, acting_efield
      use m_efield,       only : get_field_from_dipole
      use m_efield,       only : dipole_correction
      use m_efield,       only : user_specified_field

      use m_doping_uniform,       only: initialize_doping_uniform
      use m_doping_uniform,       only: compute_doping_structs_uniform,
     $                                  doping_active
      use m_rhog,                 only: rhog, rhog_in
      use m_rhog,                 only: order_rhog
      use siesta_options,         only: mix_charge
#ifdef MPI
      use mpi_siesta
#endif

      use m_mesh_node,   only: init_mesh_node
      use m_charge_add,  only: init_charge_add
      use m_hartree_add, only: init_hartree_add

      use m_ts_global_vars,only: TSmode
      use m_ts_options,   only : IsVolt, N_Elec, Elecs
      use m_ts_voltage,   only : ts_init_voltage
      use m_ts_hartree,   only : ts_init_hartree_fix

      implicit none
      integer, intent(in)     :: nspin, norb, iaorb(norb), iphorb(norb),
     &                           nuo, nuotot, nua, na, isa(na),
     &                           indxua(na), mscell(3,3), maxnd,
     &                           numd(nuo), listdptr(nuo), listd(maxnd)
      real(dp), intent(in)    :: xa(3,na), ucell(3,3), datm(norb)
      real(dp), intent(inout) :: g2max
      integer, intent(inout)  :: ntm(3)
      real(dp), intent(inout) :: Fal(3,nua), stressl(3,3)

      real(dp), parameter     :: tiny  = 1.e-12_dp
      integer                 :: io, ia, iphi, is, n, i, j
      integer                 :: nsc(3), nbcell, nsd
      real(dp)                :: DStres(3,3), volume
      real(dp), external      :: volcel, ddot
      real(grid_p)            :: dummy_Drho(1,1), dummy_Vaux(1),
     &                           dummy_Vscf(1)
      logical, save           :: frstme = .true.   ! Keeps state
      real(grid_p),   pointer :: Vscf(:,:), rhoatm_par(:)
      integer,        pointer :: numphi(:), numphi_par(:)

      integer                 :: nm(3)   ! For call to initMesh
#ifndef BSC_CELLXC
      integer                 :: nXCfunc
      character(len=20)       :: XCauth(10), XCfunc(10)
#endif /* ! BSC_CELLXC */
      ! Transport direction (unit-cell aligned)
      integer                 :: iE
      real(dp)                :: ortho, field(3), field2(3)
!--------------------------------------------------------------------- BEGIN
#ifdef DEBUG
      call write_debug( '    PRE dhscf_init' )
#endif
! ----------------------------------------------------------------------
!     General initialisation
! ----------------------------------------------------------------------
!     Start time counter
      call timer( 'DHSCF_Init', 1 )

      nsd = min(nspin,2)
      nullify(Vscf,rhoatm_par)

      if (frstme) then
        debug_dhscf = fdf_get('Debug.DHSCF', .false.)

        nullify( xdsp, rhopcc, Vna, rhoatm )
!       nsm lives in module m_dhscf now    !! AG**
        nsm = fdf_integer( 'MeshSubDivisions', 2 )
        nsm = max( nsm, 1 )

!       Set mesh sub-division variables & perform one off allocation
        nsp = nsm*nsm*nsm
        call re_alloc( xdsp, 1, 3, 1, nsp, 'xdsp', 'dhscf_init' )

!       Check spin-spiral wavevector (if defined)
        if (spiral .and. nspin.lt.4)
     &    call die('dhscf: ERROR: spiral defined but nspin < 4')
      endif   ! First time

#ifndef BSC_CELLXC
! Get functional(s) being used
      call getXC( nXCfunc, XCfunc, XCauth )
#endif /* ! BSC_CELLXC */

      if (harrisfun) then
        do n = 1,nXCfunc
          if (.not.(leqi(XCauth(n),'PZ').or.leqi(XCauth(n),'CA'))) then
            call die("** Harris forces not implemented for non-LDA XC")
          endif
        enddo
      endif

! ----------------------------------------------------------------------
!     Orbital initialisation : part 1
! ----------------------------------------------------------------------

!     Find the maximum orbital radius
      rmax = 0.0_dp
      do io = 1, norb
        ia   = iaorb(io)    ! Atomic index of each orbital
        iphi = iphorb(io)   ! Orbital index of each  orbital in its atom
        is   = isa(ia)      ! Species index of each atom
        rmax = max( rmax, rcut(is,iphi) )
      enddo

!     Start time counter for mesh initialization
      call timer( 'DHSCF1', 1 )

! ----------------------------------------------------------------------
!     Unit cell handling
! ----------------------------------------------------------------------
!     Find diagonal unit cell and supercell
      call digcel( ucell, mscell, cell, scell, nsc, IsDiag )
      if (.not.IsDiag) then
        if (Node.eq.0) then
          write(6,'(/,a,3(/,a,3f12.6,a,i6))')
     &      'DHSCF: WARNING: New shape of unit cell and supercell:',
     &      ('DHSCF:',(cell(i,j),i=1,3),'   x',nsc(j),j=1,3)
        endif
      endif

!     Find the system shape
      call shaper( cell, nua, isa, xa, shape, nbcell, bcell )

!     Find system volume
      volume = volcel( cell )

! ----------------------------------------------------------------------
!     Mesh initialization 
! ----------------------------------------------------------------------
      call InitMesh( na, cell, norb, iaorb, iphorb, isa, rmax, 
     &               G2max, G2mesh, nsc, nmpl, nm,
     &               nml, ntm, ntml, ntpl, dvol )

!     Setup box descriptors for each processor,
!     held in module iogrid_netcdf
      call set_box_limits( ntm, nsm )

      ! Initialize information on local mesh for each node
      call init_mesh_node( cell, ntm , meshLim , nsm )

      ! Setup charge additions in the mesh
      call init_charge_add(cell, ntm)

      ! Setup Hartree additions in the mesh
      call init_hartree_add(cell, ntm)

#ifdef NCDF_4
      ! Initialize the box for each node...
      call cdf_init_mesh( ntm, nsm )
#endif

!     Stop time counter for mesh initialization
      call timer( 'DHSCF1', 2 )
! ----------------------------------------------------------------------
!     End of mesh initialization 
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
!     Initialize atomic orbitals, density and potential
! ----------------------------------------------------------------------
!     Start time counter for atomic initializations
      call timer( 'DHSCF2', 1 )

      if (nodes.gt.1) then
        call setMeshDistr( UNIFORM, nsm, nsp,
     &                     nml, nmpl, ntml, ntpl )
      endif

!     Initialise quantities relating to the atom-mesh positioning
      call InitAtomMesh( UNIFORM, na, xa )

#ifdef BSC_CELLXC
!     Check if we need extencils in cellxc
      call setGGA( )

#endif /* BSC_CELLXC */
!     Compute the number of orbitals on the mesh and recompute the
!     partions for every processor in order to have a similar load
!     in each of them.
      nullify( numphi )
      call re_alloc( numphi, 1, nmpl, 'numphi', 'dhscf_init' )
!$OMP parallel do default(shared), private(i)
      do i= 1, nmpl
        numphi(i) = 0
      enddo
!$OMP end parallel do

      call distriPhiOnMesh( nm, nmpl, norb, iaorb, iphorb,
     &                      isa, numphi )

!     Find if there are partial-core-corrections for any atom
      npcc = 0
      do ia = 1,na
        if (rcore(isa(ia)) .gt. tiny) npcc = 1
      enddo

!     Find partial-core-correction energy density
!     Vscf and Vaux are not used here
      call re_alloc( rhopcc, 1, ntpl*npcc+1, 'rhopcc', 'dhscf_init' )
      if (npcc .eq. 1) then
        call PartialCoreOnMesh( na, isa, ntpl, rhopcc, indxua,
     &    nsd, dvol, volume, dummy_Vscf, dummy_Vaux, Fal, stressl,
     &    .false., .false. )
        call reord( rhopcc, rhopcc, nml, nsm, TO_SEQUENTIAL )
        if ( debug_dhscf ) then
           write(*,debug_fmt) Node,'rhopcc',sqrt(sum(rhopcc**2))
        end if
      endif

!     Find neutral-atom potential
!     Drho is not used here
      call re_alloc( Vna, 1, ntpl, 'Vna', 'dhscf_init' )
      call NeutralAtomOnMesh( na, isa, ntpl, Vna, indxua, dvol, 
     &                        volume, dummy_DRho, Fal, stressl, 
     &                        .false., .false. )
      call reord( Vna, Vna, nml, nsm, TO_SEQUENTIAL )
      if ( debug_dhscf ) then
         write(*,debug_fmt) Node,'Vna',sqrt(sum(Vna**2))
      end if

      if (nodes.gt.1) then
        if (node .eq. 0) then
           write(6,"(a)") "Setting up quadratic distribution..."
        endif
        call setMeshDistr( QUADRATIC, nsm, nsp,
     &                     nml, nmpl, ntml, ntpl )

!       Create extended mesh arrays for the second data distribution
        call setupExtMesh( QUADRATIC, rmax )

!       Compute atom positions for the second data distribution 
        call InitAtomMesh( QUADRATIC, na, xa )
      endif

!     Calculate orbital values on mesh
!     numphi has already been computed in distriPhiOnMesh
!     in the UNIFORM distribution 
      if (nodes.eq.1) then
        numphi_par => numphi
      else
        nullify(numphi_par)
        call re_alloc( numphi_par, 1, nmpl, 'numphi_par',
     &                 'dhscf_init' )
        call distMeshData( UNIFORM, numphi, QUADRATIC,
     &                     numphi_par, KEEP )
      endif

      call PhiOnMesh( nmpl, norb, iaorb, iphorb, isa, numphi_par )

      if (nodes.gt.1) then
        call de_alloc( numphi_par, 'numphi_par', 'dhscf_init' )
      endif
      call de_alloc( numphi, 'numphi', 'dhscf_init' )

! ----------------------------------------------------------------------
!       Create sparse indexing for Dscf as needed for local mesh
!       Note that this is done in the QUADRATIC distribution
!       since 'endpht' (computed finally in PhiOnMesh and stored in
!       meshphi module) is in that distribution.
! ----------------------------------------------------------------------
      if (Nodes.gt.1) then
        call CreateLocalDscfPointers( nmpl, nuotot, numd, listdptr, 
     &                                listd )
      endif

! ----------------------------------------------------------------------
!     Calculate terms relating to the neutral atoms on the mesh
! ----------------------------------------------------------------------
!     Find Harris (sum of atomic) electron density
      call re_alloc( rhoatm_par, 1, ntpl, 'rhoatm_par', 'dhscf_init' )
      call rhooda( norb, nmpl, datm, rhoatm_par, iaorb, iphorb, isa )
!     rhoatm_par comes out of here in clustered form in QUADRATIC dist

!     Routine Poison should use the uniform data distribution
      if (nodes.gt.1) then
         call setMeshDistr( UNIFORM, nsm, nsp,
     &                      nml, nmpl, ntml, ntpl )
      endif

!     Create Rhoatm using UNIFORM distr, in sequential form
      call re_alloc( rhoatm, 1, ntpl, 'rhoatm', 'dhscf_init' )
      call distMeshData( QUADRATIC, rhoatm_par,
     &     UNIFORM, rhoatm, TO_SEQUENTIAL )

      if ( debug_dhscf ) then
         write(*,debug_fmt) Node,'rhoatm', sqrt(sum(rhoatm**2))
      end if

!
!  AG: The initialization of doping structs could be done here now,
!      in the uniform distribution, and with a simple loop over
!      rhoatm.

        if (frstme) call initialize_doping_uniform()
        if (doping_active) then  
           call compute_doping_structs_uniform(ntpl,rhoatm,nsd)
           ! Will get the global number of hit points
           ! Then, the doping density to be added can be simply computed
        endif

!     Allocate Temporal array
      call re_alloc( Vscf, 1, ntpl, 1, nspin, 'Vscf', 'dhscf_init' )

!     Vscf is filled here but not used later
!     Uharrs is computed (and saved)
!     DStres is computed but not used later
      call poison( cell, ntml(1), ntml(2), ntml(3), ntm, rhoatm,
     &             Uharrs, Vscf, DStres, nsm )
      call de_alloc( Vscf, 'Vscf', 'dhscf_init' )

!     Always deallocate rhoatm_par, as it was used even if nodes=1
      call de_alloc( rhoatm_par, 'rhoatm_par', 'dhscf_init' )

      if (mix_charge) then
         call re_alloc( rhog, 1, 2, 1, ntpl, 1, nspin,
     $                 'Rhog', 'dhscf_init' )
         call re_alloc( rhog_in, 1, 2, 1, ntpl, 1, nspin,
     $                 'Rhog_in', 'dhscf_init' )
         call order_rhog(cell, ntml(1), ntml(2), ntml(3), ntm, nsm)
      endif

!     Stop time counter for atomic initializations
      call timer( 'DHSCF2', 2 )

! ----------------------------------------------------------------------
!     At the end of initializations:
!     We are in the UNIFORM distribution
!     Rhoatm, Rhopcc and Vna are in UNIFORM dist, sequential form
!     The index array endpht is in the QUADRATIC distribution
! ----------------------------------------------------------------------
      if (frstme) then
         call initialize_efield()
      end if

      ! Check if we need to add the potential 
      ! corresponding to the voltage-drop.
      if ( TSmode ) then
        ! These routines are important if there are cell-changes
        call ts_init_hartree_fix( cell, nua, xa, ntm, ntml)
        if ( IsVolt ) then
           call ts_init_voltage( cell, nua, xa, ntm)
        end if

        if ( acting_efield ) then
         ! We do not allow the electric field for 
         ! transiesta runs with V = 0, either.
         ! It does not make sense, only for fields perpendicular
         ! to the applied bias.
           
         ! We need to check that the e-field is perpendicular
         ! to the transport direction, and that the system is
         ! either a chain, or a slab.
         ! However, due to the allowance of a dipole correction
         ! along the transport direction for buffer calculations
         ! we have to allow all shapes. (atom is not transiesta
         ! compatible anyway)
            
         ! check that we do not violate the periodicity
         if ( Node .eq. 0 ) then
            write(*,'(/,2(2a,/))') 'ts-WARNING: ',
     &           'E-field/dipole-correction! ',
     &           'ts-WARNING: ',
     &           'I hope you know what you are doing!'
         end if

         ! This is either dipole or user, or both
         field(:) = user_specified_field(:)
         do iE = 1 , N_Elec
            field2 = Elecs(iE)%cell(:,Elecs(iE)%t_dir)
            ortho = ddot(3,field2,1,field,1)
            if ( abs(ortho) > 1.e-9_dp ) then
               call die('User defined E-field must be
     &perpendicular to semi-infinite directions')
            end if
         end do
        end if ! acting_efield
         
        ! We know that we currently allow people to do more than
        ! they probably should be allowed. However, there are many
        ! corner cases that may require dipole corrections, or 
        ! electric fields to "correct" an intrinsic dipole.
        ! For instance, what should we do with a dipole in a transiesta
        ! calculation?
        ! Should we apply a field to counter act it in a device
        ! calculation?
         
      end if

      frstme = .false.

      call timer( 'DHSCF_Init', 2 )
#ifdef DEBUG
      call write_debug( '    POS dhscf_init' )
#endif
!------------------------------------------------------------------------- END
      end subroutine dhscf_init

      subroutine dhscf( nspin, norb, iaorb, iphorb, nuo, 
     &                  nuotot, nua, na, isa, xa, indxua,
     &                  ntm, ifa, istr, iHmat,
     &                  filesOut, maxnd, numd,
     &                  listdptr, listd, Dscf, datm, maxnh, Hmat,
     &                  Enaatm, Enascf, Uatm, Uscf, DUscf, DUext, 
     &                  Exc, Dxc, dipol, stress, Fal, stressl,
     &                  use_rhog_in, charge_density_only )
!! author: J.M. Soler
!! date: August 1996
!!
!! Calculates the self-consistent field contributions to Hamiltonian
!! matrix elements, total energy and atomic forces.
!!
!! Coded by J.M. Soler, August 1996. July 1997.  
!! Modified by J.D. Gale, February 2000.
!!
!!
!!### Units
!! Energies in Rydbergs.  
!! Distances in Bohr.
!!
!!### Routines called internally
!! * [[cellxc(proc)]]    : Finds total exch-corr energy and potential
!! * [[cross(proc)]]   : Finds the cross product of two vectors
!! * [[dfscf(proc)]]     : Finds SCF contribution to atomic forces
!! * [[dipole(proc)]]    : Finds electric dipole moment
!! * [[doping(proc)]]    : Adds a background charge for doped systems
!! * [[write_rho(proc)]]     : Saves electron density on a file
!! * [[poison(proc)]]    : Solves Poisson equation
!! * [[reord(proc)]]     : Reorders electron density and potential arrays
!! * [[rhooda(proc)]]    : Finds Harris electron density in the mesh
!! * [[rhoofd(proc)]]    : Finds SCF electron density in the mesh
!! * [[rhoofdsp(proc)]]  : Finds SCF electron density in the mesh for
!!                    spiral arrangement of spins
!! * [[timer(proc)]]     : Finds CPU times
!! * [[vmat(proc)]]      : Finds matrix elements of SCF potential
!! * [[vmatsp(proc)]]    : Finds matrix elements of SCF potential for
!!                         spiral arrangement of spins
!! * [[delk(proc)]] : Finds matrix elements of \( exp(i \vec{k} \cdot \vec{r}) \)
!! * real*8 volcel( cell ) : Returns volume of unit cell
!!
!!### Internal variables and arrays
!! * `real*8  bcell(3,3)`    : Bulk lattice vectors
!! * `real*8  cell(3,3)`     : Auxiliary lattice vectors (same as ucell)
!! * `real*8  const`         : Auxiliary variable (constant within a loop)
!! * `real*8  DEc`           : Auxiliary variable to call cellxc
!! * `real*8  DEx`           : Auxiliary variable to call cellxc
!! * `real*8  dvol`          : Mesh-cell volume
!! * `real*8  Ec`            : Correlation energy
!! * `real*8  Ex`            : Exchange energy
!! * `real*8  field(3)`      : External electric field
!! * `integer i`             : General-purpose index
!! * `integer ia`            : Atom index
!! * `integer io`            : Orbital index
!! * `integer ip`            : Point index
!! * `integer is`            : Species index
!! * `logical IsDiag`        : Is supercell diagonal?
!! * `integer ispin`         : Spin index
!! * `integer j`             : General-purpose index
#ifndef BSC_CELLXC
!! * `integer JDGdistr`      : J.D.Gale's parallel distribution of mesh points
!! * `integer myBox(2,3)`    : My processor's mesh box
#endif /* ! BSC_CELLXC */
!! * `integer nbcell`        : Number of independent bulk lattice vectors
!! * `integer npcc`          : Partial core corrections? (0=no, 1=yes)
!! * `integer nsd`           : Number of diagonal spin values (1 or 2)
!! * `integer ntpl`          : Number of mesh Total Points in unit cell
!!                           (including subpoints) locally
!! * `real*4  rhoatm(ntpl)`  : Harris electron density
!! * `real*4  rhopcc(ntpl)`  : Partial-core-correction density for xc
!! * `real*4  DRho(ntpl)`    : Selfconsistent electron density difference
!! * `real*8  rhotot`        : Total density at one point
!! * `real*8  rmax`          : Maximum orbital radius
!! * `real*8  scell(3,3)`    : Supercell vectors
!! * `character shape*10`    : Name of system shape
!! * `real*4  Vaux(ntpl)`    : Auxiliary potential array
!! * `real*4  Vna(ntpl)`     : Sum of neutral-atom potentials
!! * `real*8  volume`        : Unit cell volume
!! * `real*4  Vscf(ntpl)`    : Hartree potential of selfconsistent density
!! * `real*8  x0(3)`         : Center of molecule
!! * `logical harrisfun`     : Harris functional or Kohn-Sham?

      use precision,     only  : dp, grid_p
#ifndef BSC_CELLXC
      use parallel, only : ProcessorY
#endif /* ! BSC_CELLXC */

!     Number of Mesh divisions of each cell vector (global)
!     The status of this variable is confusing
      use parallel,      only  : Node, Nodes
      use atmfuncs,      only  : rcut, rcore
      use units,         only  : Debye, eV, Ang
      use fdf
      use sys,           only  : die, bye
      use mesh,          only  : nsm, nsp
      use parsing
      use m_iorho,       only  : write_rho
      use m_forhar,      only  : forhar
      use alloc,         only  : re_alloc, de_alloc
      use files,         only  : slabel
      use files,         only  : filesOut_t ! derived type for output file names
      use siesta_options, only : harrisfun, save_initial_charge_density
      use siesta_options, only : analyze_charge_density_only
      use meshsubs,       only : LocalChargeOnMesh
      use meshsubs,       only : PartialCoreOnMesh
      use meshsubs,       only : NeutralAtomOnMesh

      use moreMeshSubs,   only : setMeshDistr, distMeshData
      use moreMeshSubs,   only : UNIFORM, QUADRATIC, LINEAR
      use moreMeshSubs,   only : TO_SEQUENTIAL, TO_CLUSTER, KEEP
      use m_partial_charges, only: compute_partial_charges
      use m_partial_charges, only: want_partial_charges
#ifndef BSC_CELLXC

      use siestaXC, only : cellXC       ! Finds xc energy and potential
      use siestaXC, only : myMeshBox    ! Returns my processor mesh box
      use siestaXC, only : jms_setMeshDistr => setMeshDistr
                                        ! Sets a distribution of mesh
                                        ! points over parallel processors
#endif /* BSC_CELLXC */
      use m_vmat,  only  : vmat
      use m_rhoofd, only: rhoofd
#ifdef MPI
      use mpi_siesta
#endif
      use iogrid_netcdf, only: write_grid_netcdf
      use iogrid_netcdf, only: read_grid_netcdf
      use siesta_options, only: read_charge_cdf
      use siesta_options, only: savebader
      use siesta_options, only: read_deformation_charge_cdf
      use siesta_options, only: mix_charge

      use m_efield,       only: get_field_from_dipole, dipole_correction
      use m_efield,       only: add_potential_from_field
      use m_efield,       only: user_specified_field, acting_efield
      use m_doping_uniform,       only: doping_active, doping_uniform
      
      use m_charge_add,   only: charge_add
      use m_hartree_add,  only: hartree_add

#ifdef NCDF_4
      use siesta_options, only: write_cdf
      use m_ncdf_siesta, only: cdf_save_grid
#endif
      use m_rhofft,       only: rhofft, FORWARD, BACKWARD
      use m_rhog,         only: rhog_in, rhog
      use m_spin,         only: spin
      use m_spin,         only: Spiral, qSpiral
      use m_iotddft,      only: write_tdrho
      use m_ts_global_vars,only: TSmode, TSrun
      use m_ts_options, only: IsVolt, Elecs, N_elec
      use m_ts_voltage, only: ts_voltage
      use m_ts_hartree, only: ts_hartree_fix

      implicit none

      integer, intent(in) :: nspin
        !! Number of different spin polarisations:  
        !! nspin=1 => Unpolarized, nspin=2 => polarized  
        !! nspin=4 => Noncollinear spin or spin-orbit.
      integer, intent(in) :: norb
        !! Total number of basis orbitals in supercell
      integer, intent(in) :: iaorb(norb)
        !! Atom to which each orbital belongs
      integer, intent(in) :: iphorb(norb)
        !! Orbital index (within atom) of each orbital
      integer, intent(in) :: nuo
        !! Number of orbitals in a unit cell in this node
      integer, intent(in) :: nuotot
        !! Number of orbitals in a unit cell
      integer, intent(in) :: nua
        !! Number of atoms in unit cell
      integer, intent(in) :: na
        !! Number of atoms in supercell
      integer, intent(in) :: isa(na)
        !! Species index of all atoms in supercell
      integer, intent(in) :: indxua(na)
        !! Index of equivalent atom in unit cell
      integer, intent(in) :: ifa
        !! Switch which fixes whether the SCF contrib:  
        !! to atomic forces is calculated and added to fa.
      integer, intent(in) :: istr
        !! Switch which fixes whether the SCF contrib:  
        !! to stress is calculated and added to stress.
      integer, intent(in) :: iHmat
        !! Switch which fixes whether the Hmat matrix
        !! elements are calculated or not.
      integer, intent(in) :: maxnd
        !! First dimension of listd and Dscf
      integer, intent(in) :: numd(nuo)
        !! Number of nonzero density-matrix
        !! elements for each matrix row
      integer, intent(in) :: listdptr(nuo)
        !! Pointer to start of rows of density-matrix
      integer, intent(in) :: listd(*)
        !! `listd(maxnd)`: Nonzero-density-matrix-element column
        !! indexes for each matrix row
      integer, intent(in) :: maxnh
        !! First dimension of listh and Hmat
      real(dp), intent(in) :: xa(3,na)
        !! Atomic positions of all atoms in supercell
      real(dp), intent(in) :: Dscf(:,:)
        !! `Dscf(maxnd,h_spin_dim)`:  
        !! SCF density-matrix elements
      real(dp), intent(in) :: datm(norb)
        !! Harris density-matrix diagonal elements  
        !! (atomic occupation charges of orbitals)
      real(dp), intent(in) :: Hmat(:,:)
        !! `Hmat(maxnh,h_spin_dim)`:  
        !! Hamiltonian matrix in sparse form,  
        !! to which are added the matrix elements  
        !! `<ORB_I | DeltaV | ORB_J>`, where  
        !! `DeltaV = Vna + Vxc(SCF) + Vhartree(RhoSCF-RhoHarris)`

      type(filesOut_t), intent(inout) :: filesOut
        !! Output file names (If blank => not saved)
      integer, intent(inout) :: ntm(3)
        !! Number of mesh divisions of each cell
        !! vector, including subgrid.
      real(dp), intent(inout) :: Fal(3,nua)
        !! Atomic forces, to which the SCF contribution
        !! is added by this routine when `ifa=1`.
        !! The SCF contribution is minus the derivative
        !! of `(Enascf - Enaatm + DUscf + Exc)` with
        !! respect to atomic positions, in Ry/Bohr.
        !! Contributions local to this node.
      real(dp), intent(inout) :: stressl(3,3)
        !! Stress tensor, to which the SCF contribution
        !! is added by this routine when `ifa=1`.
        !! The SCF contribution is minus the derivative of
        !! `(Enascf - Enaatm + DUscf + Exc) / volume`
        !! with respect to the strain tensor, in Ry.
        !! Contributions local to this node.
      real(dp) :: stress(3,3)

      real(dp), intent(out) :: Enaatm
        !! Integral of `Vna * rhoatm`
      real(dp), intent(out) :: Enascf
        !! Integral of `Vna * rhoscf`
      real(dp), intent(out) :: Uatm
        !! Harris hartree electron-interaction energy
      real(dp), intent(out) :: Uscf
        !! SCF hartree electron-interaction energy
      real(dp), intent(out) :: DUscf
        !! Electrostatic (Hartree) energy of
        !! `(rhoscf - rhoatm)` density
      real(dp), intent(out) :: DUext
        !! Interaction energy with external electric field
      real(dp), intent(out) :: Exc
        !! SCF exchange-correlation energy
      real(dp), intent(out) :: Dxc
        !! SCF double-counting correction to Exc  
        !! `Dxc = integral of ( (epsxc - Vxc) * Rho )`  
        !! All energies in Rydbergs
      real(dp), intent(out) :: dipol(3)
        !! Electric dipole (in a.u.)
        !! only when the system is a molecule

      logical, intent(in), optional :: use_rhog_in
      logical, intent(in), optional :: charge_density_only


!     Local variables
      integer  :: i, ia, ip, ispin, nsd, np_vac
#ifndef BSC_CELLXC
!     Interface to JMS's SiestaXC
      integer       :: myBox(2,3)
      integer, save :: JDGdistr=-1
      real(dp) :: stressXC(3,3)
#endif /* ! BSC_CELLXC */
      real(dp) :: b1Xb2(3), const, DEc, DEx, DStres(3,3),
     &            Ec, Ex, rhotot, x0(3), volume, Vmax_vac, Vmean_vac
#ifdef BSC_CELLXC
!     Dummy arrays for cellxc call
      real(grid_p) :: aux3(3,1)
      real(grid_p) :: dummy_DVxcdn(1,1,1)
#endif /* BSC_CELLXC */

      logical :: use_rhog

      real(dp), external :: volcel, ddot

      external
     &  cross,
     &  dipole, 
     &  poison, 
     &  reord, rhooda, rhoofdsp, 
     &  timer, vmatsp, 
     &  readsp
#ifdef BSC_CELLXC
      external bsc_cellxc
#endif /* BSC_CELLXC */

!     Work arrays
      real(grid_p), pointer :: Vscf(:,:), Vscf_par(:,:),
     &                         DRho(:,:), DRho_par(:,:),
     &                         Vaux(:), Vaux_par(:), Chlocal(:),
     &                         Totchar(:), fsrc(:), fdst(:),
     &                         rhoatm_quad(:) => null(),
     &                         DRho_quad(:,:) => null()
      ! Temporary reciprocal spin quantity
      real(grid_p) :: rnsd
#ifdef BSC_CELLXC
      real(grid_p), pointer :: Vscf_gga(:,:), DRho_gga(:,:)
#endif /* BSC_CELLXC */
#ifdef MPI
      integer  :: MPIerror
      real(dp) :: sbuffer(7), rbuffer(7)
#endif

#ifdef DEBUG
      call write_debug( '    PRE DHSCF' )
#endif

      if ( spin%H /= size(Dscf,dim=2) ) then
         call die('Spin components is not equal to options.')
      end if

      if ( debug_dhscf ) then
         write(*,debug_fmt) Node,'DM',
     &        (sqrt(sum(Dscf(:,ispin)**2)),ispin=1,spin%H)
         write(*,debug_fmt) Node,'H',
     &        (sqrt(sum(Hmat(:,ispin)**2)),ispin=1,spin%H)
      end if
      
!-------------------------------------------------------------------- BEGIN
! ----------------------------------------------------------------------
! Start of SCF iteration part
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
!     At the end of DHSCF_INIT, and also at the end of any previous
!     call to dhscf, we were in the UNIFORM distribution
!     Rhoatm, Rhopcc and Vna were in UNIFORM dist, sequential form
!     The index array endpht was in the QUADRATIC distribution
! ----------------------------------------------------------------------

#ifdef _TRACE_
      call MPI_Barrier( MPI_Comm_World, MPIerror )
      call MPItrace_restart( )
#endif
      call timer( 'DHSCF', 1 )
      call timer( 'DHSCF3', 1 )

      nullify( Vscf, Vscf_par, DRho, DRho_par,
     &         Vaux, Vaux_par, Chlocal, Totchar )
#ifdef BSC_CELLXC
      nullify( Vscf_gga, DRho_gga)
#endif /* BSC_CELLXC */

      volume = volcel(cell)

!-------------------------------------------------------------------------
      
      if (analyze_charge_density_only) then
         !! Use the functionality in the first block
         !! of the routine to get charge files and partial charges
         call setup_analysis_options()
      endif
      
      if (filesOut%vna .ne. ' ') then
        ! Uniform dist, sequential form
#ifdef NCDF_4
        if ( write_cdf ) then
           call cdf_save_grid(trim(slabel)//'.nc','Vna',1,ntml,Vna)
        else
           call write_rho( filesOut%vna, 
     &          cell, ntm, nsm, ntpl, 1, Vna)
           call write_grid_netcdf( cell, ntm, 1, ntpl, Vna, "Vna")
        end if
#else
        call write_rho( filesOut%vna,
     &       cell, ntm, nsm, ntpl, 1, Vna )
        call write_grid_netcdf( cell, ntm, 1, ntpl, Vna, "Vna")
#endif
      endif

!     Allocate memory for DRho using the UNIFORM data distribution
      call re_alloc( DRho, 1, ntpl, 1, nspin, 'DRho','dhscf' )

! Find number of diagonal spin values
      nsd  = min( nspin, 2 )
      if ( nsd == 1 ) then
         rnsd = 1._grid_p
      else
         rnsd = 1._grid_p / nsd
      end if

! ----------------------------------------------------------------------
! Find SCF electron density at mesh points. Store it in array DRho
! ----------------------------------------------------------------------
!
!     The reading routine works in the uniform distribution, in
!     sequential form
!
      if (present(use_rhog_in)) then
            use_rhog = use_rhog_in
         else
            use_rhog = .false.
      endif
      if (use_rhog) then
         ! fourier transform back into drho
         call rhofft(cell, ntml(1), ntml(2), ntml(3), ntm, nspin,
     $                  DRho,rhog_in,BACKWARD)

      else if (read_charge_cdf) then
         call read_grid_netcdf(ntm(1:3),nspin,ntpl,DRho,"Rho")
         read_charge_cdf = .false.
      else if (read_deformation_charge_cdf) then
         call read_grid_netcdf(ntm(1:3),nspin,ntpl,DRho,"DeltaRho")
         ! Add to diagonal components only
         do ispin = 1,nsd
            do ip= 1, ntpl
!             rhoatm and Drho are in sequential mode
              DRho(ip,ispin) = DRho(ip,ispin) + rhoatm(ip) * rnsd
            enddo
         enddo
         read_deformation_charge_cdf = .false.
      else
        ! Set the QUADRATIC distribution and allocate memory for DRho_par
        ! since the construction of the density from the DM and orbital
        ! data needs that distribution
        if (nodes.gt.1) then
           call setMeshDistr( QUADRATIC, nsm, nsp,
     &                        nml, nmpl, ntml, ntpl )
        endif
        call re_alloc( DRho_par, 1, ntpl, 1, nspin,
     &                 'DRho_par','dhscf' )       
        if (Spiral) then
          call rhoofdsp( norb, nmpl, maxnd, numd, listdptr, listd,
     &                   nspin, Dscf, DRho_par, nuo, nuotot, iaorb,
     &                   iphorb, isa, qspiral )
        else
          call rhoofd( norb, nmpl, maxnd, numd, listdptr, listd,
     &                 nspin, Dscf, DRho_par, 
     &                 nuo, nuotot, iaorb, iphorb, isa )
        endif
        ! DRHO_par is here in QUADRATIC, clustered form

!       Set the UNIFORM distribution again and copy DRho to it
        if (nodes.gt.1) then
           call setMeshDistr( UNIFORM, nsm, nsp, nml, nmpl, ntml, ntpl )
        endif

        do ispin = 1, nspin
          fsrc => DRho_par(:,ispin)
          fdst => DRho(:,ispin)
         ! Sequential to be able to write it out
         ! if nodes==1, this call will just reorder
          call distMeshData( QUADRATIC, fsrc,
     &                       UNIFORM, fdst, TO_SEQUENTIAL )
        enddo
        call de_alloc( DRho_par, 'DRho_par','dhscf' )

        if ( debug_dhscf ) then
           write(*,debug_fmt) Node,'DRho',
     &          (sqrt(sum(DRho(:,ispin)**2)),ispin=1,nspin)
        end if

        if (save_initial_charge_density) then
           ! This section is to be deprecated in favor
           ! of "analyze_charge_density_only"
           ! (except for the special name for the .nc file)
#ifdef NCDF_4
          if ( write_cdf ) then
             call cdf_save_grid(trim(slabel)//'.nc','RhoInit',nspin,
     &            ntml,DRho)
          else
             call write_rho( "RHO_INIT", cell, ntm, nsm, ntpl,
     $            nspin, DRho)
             call write_grid_netcdf( cell, ntm, nspin, ntpl, DRho,
     $            "RhoInit")
          end if
#else
          call write_rho( "RHO_INIT", cell, ntm, nsm, ntpl,
     $         nspin, DRho)
          call write_grid_netcdf( cell, ntm, nspin, ntpl, DRho,
     $         "RhoInit")
#endif
          call timer('DHSCF3',2)
          call timer('DHSCF',2)
          call bye("STOP after producing RHO_INIT from input DM")
        endif

      endif

      if (mix_charge) then
         ! Save fourier transform of charge density
         call rhofft(cell, ntml(1), ntml(2), ntml(3), ntm, nspin,
     $        DRho,rhog,FORWARD)
      endif
!
!     Proper place to integrate Hirshfeld and Voronoi code,
!     since we have just computed rhoatm and Rho.

      if (want_partial_charges) then
        ! The endpht array is in the quadratic distribution, so
        ! we need to use it for this...
        if (nodes.gt.1) then
           call setMeshDistr( QUADRATIC, nsm, nsp,
     &                        nml, nmpl, ntml, ntpl )
        endif
        call re_alloc( DRho_quad, 1, ntpl, 1, nspin,
     &                 'DRho_quad','dhscf' )       
        call re_alloc( rhoatm_quad, 1, ntpl,
     &                 'rhoatm_quad','dhscf' )
        ! Redistribute grid-density
        do ispin = 1, nspin
          fsrc => DRho(:,ispin)
          fdst => DRho_quad(:,ispin)
         ! if nodes==1, this call will just reorder
          call distMeshData( UNIFORM, fsrc,
     &                       QUADRATIC, fdst, TO_CLUSTER )
        enddo
        call distMeshData( UNIFORM, rhoatm,
     &                     QUADRATIC, rhoatm_quad, TO_CLUSTER )

        call compute_partial_charges(DRho_quad,rhoatm_quad,
     .                  nspin, iaorb, iphorb, 
     .                  isa, nmpl,dvol)

        call de_alloc(rhoatm_quad,'rhoatm_quad','dhscf')
        call de_alloc(Drho_quad,'DRho_quad','dhscf')
        if (nodes.gt.1) then
           call setMeshDistr( UNIFORM, nsm, nsp,
     &                        nml, nmpl, ntml, ntpl )
        endif
      endif

! ----------------------------------------------------------------------
! Save electron density
! ----------------------------------------------------------------------
      if (filesOut%rho .ne. ' ') then
        !  DRho is already using a uniform, sequential form
#ifdef NCDF_4
        if ( write_cdf ) then
           call cdf_save_grid(trim(slabel)//'.nc','Rho',nspin,ntml,
     &          DRho)
        else
           call write_rho( filesOut%rho, cell, ntm, nsm, ntpl, nspin,
     &          DRho )
           call write_grid_netcdf( cell, ntm, nspin, ntpl, DRho, "Rho")
        end if
#else
        call write_rho( filesOut%rho, cell, ntm, nsm, ntpl, nspin,
     &       DRho )
        call write_grid_netcdf( cell, ntm, nspin, ntpl, DRho, "Rho")
#endif
      endif
!-----------------------------------------------------------------------
! Save TD-electron density after every given number of steps- Rafi, Jan 2016
!-----------------------------------------------------------------------
      call write_tdrho(filesOut)
      if (filesOut%tdrho .ne. ' ') then
        !  DRho is already using a uniform, sequential form
        call write_rho( filesOut%tdrho, cell, ntm, nsm, ntpl, nspin,
     &                  DRho )
        call write_grid_netcdf( cell, ntm, nspin, ntpl, DRho, "TDRho")
      endif
! ----------------------------------------------------------------------
! Save the diffuse ionic charge and/or the total (ionic+electronic) charge
! ----------------------------------------------------------------------
      if (filesOut%psch .ne. ' ' .or. filesOut%toch .ne. ' ') then
!       Find diffuse ionic charge on mesh
        ! Note that the *OnMesh routines, except PhiOnMesh,
        ! work with any distribution, thanks to the fact that
        ! the ipa, idop, and indexp arrays are distro-specific
        call re_alloc( Chlocal, 1, ntpl, 'Chlocal', 'dhscf' )
        call LocalChargeOnMesh( na, isa, ntpl, Chlocal, indxua )
        ! Chlocal comes out in clustered form, so we convert it
        call reord( Chlocal, Chlocal, nml, nsm, TO_SEQUENTIAL )

        if ( debug_dhscf ) then
           write(*,debug_fmt) Node,'Chlocal',sqrt(sum(Chlocal**2))
        end if

!       Save diffuse ionic charge 
        if (filesOut%psch .ne. ' ') then
#ifdef NCDF_4
          if ( write_cdf ) then
             call cdf_save_grid(trim(slabel)//'.nc','Chlocal',1,ntml,
     &            Chlocal)
          else
             call write_rho( filesOut%psch, cell, ntm, nsm, ntpl, 1,
     &            Chlocal)
             call write_grid_netcdf( cell, ntm, 1, ntpl, Chlocal,
     &            'Chlocal' )
          end if
#else
          call write_rho( filesOut%psch, cell, ntm, nsm, ntpl, 1,
     &         Chlocal)
          call write_grid_netcdf( cell, ntm, 1, ntpl,
     &         Chlocal, 'Chlocal' )
#endif
        endif

!       Save total (ionic+electronic) charge 
        if ( filesOut%toch .ne. ' ') then
           ! *****************
           ! **  IMPORTANT  **
           ! The Chlocal array is re-used to minimize memory
           ! usage. In the this small snippet the Chlocal
           ! array will contain the total charge, and
           ! if the logic should change, (i.e. should Chlocal
           ! be retained) is the Totchar needed to be re-instantiated.
           ! *****************

!$OMP parallel default(shared), private(ispin,ip)
           do ispin = 1, nsd
!$OMP do 
           do ip = 1, ntpl
              Chlocal(ip) = Chlocal(ip) + DRho(ip,ispin)
           end do
!$OMP end do 
           end do
!$OMP end parallel

          ! See note above
#ifdef NCDF_4
           if ( write_cdf ) then
              call cdf_save_grid(trim(slabel)//'.nc','RhoTot',1,ntml,
     &             Chlocal)
           else
              call write_rho(filesOut%toch,cell,ntm,nsm,ntpl,1,Chlocal)
              call write_grid_netcdf( cell, ntm, 1, ntpl, Chlocal, 
     &             "TotalCharge")
           end if
#else
           call write_rho( filesOut%toch, cell, ntm, nsm, ntpl, 1,
     &          Chlocal )
           call write_grid_netcdf( cell, ntm, 1, ntpl,
     &          Chlocal, "TotalCharge")
#endif
        end if 
        call de_alloc( Chlocal, 'Chlocal', 'dhscf' )
      endif

! ----------------------------------------------------------------------
! Save the total charge (model core + valence) for Bader analysis
! ----------------------------------------------------------------------
      
      ! The test for toch guarantees that we are in "analysis mode"
      if (filesOut%toch .ne. ' ' .and. savebader) then
        call save_bader_charge()
      endif

! Find difference between selfconsistent and atomic densities

      !Both DRho and rhoatm are using a UNIFORM, sequential form
!$OMP parallel do default(shared), private(ispin,ip),
!$OMP&collapse(2)
      do ispin = 1,nsd
        do ip = 1,ntpl
          DRho(ip,ispin) = DRho(ip,ispin) - rhoatm(ip) * rnsd
        enddo
      enddo
!$OMP end parallel do

! ----------------------------------------------------------------------
! Save electron density difference
! ----------------------------------------------------------------------
      if (filesOut%drho .ne. ' ') then
#ifdef NCDF_4
        if ( write_cdf ) then
           call cdf_save_grid(trim(slabel)//'.nc','RhoDelta',nspin,ntml,
     &          DRho)
        else
           call write_rho( filesOut%drho, cell, ntm, nsm, ntpl, nspin,
     &          DRho )
           call write_grid_netcdf( cell, ntm, nspin, ntpl, DRho, 
     &          "DeltaRho")
        end if
#else
        call write_rho( filesOut%drho, cell, ntm, nsm, ntpl, nspin,
     &       DRho )
        call write_grid_netcdf( cell, ntm, nspin, ntpl,
     &       DRho, "DeltaRho")
#endif
      endif

      if (present(charge_density_only)) then 
         if (charge_density_only) then
            call timer('DHSCF3',2)
            call timer('DHSCF',2)
            call de_alloc( DRho, 'DRho', 'dhscf' )
            RETURN
         endif
      endif

! End of analysis section
! Can exit now, if requested
      
        if (analyze_charge_density_only) then 
            call timer('DHSCF3',2)
            call timer('DHSCF',2)
           call bye("STOP after analyzing charge from input DM")
        endif

!-------------------------------------------------------------
!     Transform spin density into sum and difference

      ! TODO Check for NC/SO:
      ! Should we diagonalize locally at every point first?
      if (nsd .eq. 2) then
!$OMP parallel do default(shared), private(rhotot,ip)
        do ip = 1,ntpl
          rhotot     = DRho(ip,1) + DRho(ip,2)
          DRho(ip,2) = DRho(ip,2) - DRho(ip,1)
          DRho(ip,1) = rhotot
        enddo
!$OMP end parallel do
      endif

! Add a background charge to neutralize the net charge, to
! model doped systems. It only adds the charge at points
! where there are atoms (i.e., not in vacuum).          
! First, call with 'task=0' to add background charge    
      if (doping_active) call doping_uniform(cell,ntpl,0,
     $     DRho(:,1),rhoatm)
      
      ! Add doping in cell (from ChargeGeometries/Geometry.Charge)
      ! Note that this routine will return immediately if no dopant is present
      call charge_add('+',cell, ntpl, DRho(:,1) )

! ----------------------------------------------------------------------
! Calculate the dipole moment
! ----------------------------------------------------------------------
      dipol(1:3) = 0.0_dp
      if (shape .ne. 'bulk') then

! Find center of system
        x0(1:3) = 0.0_dp
        do ia = 1,nua
          x0(1:3) = x0(1:3) + xa(1:3,ia) / nua
        enddo

! Find dipole
        ! This routine is distribution-blind
        ! and will reduce over all processors.
        call dipole( cell, ntm, ntml(1), ntml(2), ntml(3), nsm,
     &               DRho, x0, dipol )

        ! Orthogonalize dipole to bulk directions
        if (shape .eq. 'chain') then
          const = ddot(3,dipol,1,bcell,1) / ddot(3,bcell,1,bcell,1)
          dipol(1:3) = dipol(1:3) - const * bcell(1:3,1)
        else if (shape .eq. 'slab') then
          call cross( bcell(1,1), bcell(1,2), b1Xb2 )
          const = ddot(3,dipol,1,b1Xb2,1) / ddot(3,b1Xb2,1,b1Xb2,1)
          dipol(1:3) = const * b1Xb2(1:3)
       end if

       if ( TSmode ) then
         if ( N_elec > 1 ) then
          ! Orthogonalize dipole to electrode transport directions
          do ia = 1 , N_Elec
            x0 = Elecs(ia)%cell(:,Elecs(ia)%t_dir)
            const = ddot(3,dipol,1,x0,1) / ddot(3,x0,1,x0,1)
            dipol(1:3) = dipol(1:3) - const * x0
          end do
        else if ( (shape == 'molecule') .or. (shape == 'chain') ) then
          ! Only allow dipole correction for chains and molecules
          ! along the semi-infinite direciton.
          ! Note this is *only* for 1-electrode setups
          ! Note that since the above removes the periodic directions
          ! this should not do anything for 'chain' with the same semi-infinite
          ! direction
          x0 = Elecs(1)%cell(:,Elecs(1)%t_dir)
          const = ddot(3,dipol,1,x0,1) / ddot(3,x0,1,x0,1)
          dipol(1:3) = const * x0
        end if
       end if
          
      endif

! ----------------------------------------------------------------------
!     Find Hartree potential of DRho = rhoscf-rhoatm. Store it in Vaux
! ----------------------------------------------------------------------
!     Solve Poisson's equation
      call re_alloc( Vaux, 1, ntpl, 'Vaux', 'dhscf' )

      call poison( cell, ntml(1), ntml(2), ntml(3), ntm, DRho,
     &             DUscf, Vaux, DStres, nsm )

      if ( debug_dhscf ) then
         write(*,debug_fmt) Node,'Poisson',sqrt(sum(Vaux(:)**2))
      end if

      ! Vscf is in the UNIFORM, sequential form, and only using
      ! the first spin index

      ! We require that even the SIESTA potential is "fixed"
      ! NOTE, this will only do something if
      !   TS.Hartree.Fix is set
      call ts_hartree_fix( ntm, ntml, Vaux)
      
! Add contribution to stress from electrostatic energy of rhoscf-rhoatm
      if (istr .eq. 1) then
        stressl(1:3,1:3) = stressl(1:3,1:3) + DStres(1:3,1:3)
      endif

! ----------------------------------------------------------------------
!     Find electrostatic (Hartree) energy of full SCF electron density
!     using the original data distribution 
! ----------------------------------------------------------------------
      Uatm = Uharrs
      Uscf = 0._dp
!$OMP parallel do default(shared), private(ip), 
!$OMP&reduction(+:Uscf)
      do ip = 1, ntpl
        Uscf = Uscf + Vaux(ip) * rhoatm(ip)
      enddo
!$OMP end parallel do
      Uscf = Uscf * dVol + Uatm + DUscf

! Call doping with 'task=1' to remove background charge added previously
! The extra charge thus only affects the Hartree energy and potential,
! but not the contribution to Enascf ( = \Int_{Vna*\rho})
      if (doping_active) call doping_uniform(cell,ntpl,1,
     $     DRho(:,1),rhoatm)

      ! Remove doping in cell (from ChargeGeometries/Geometry.Charge)
      ! Note that this routine will return immediately if no dopant is present
      call charge_add('-',cell, ntpl, DRho(:,1) )

! ----------------------------------------------------------------------
! Add neutral-atom potential to Vaux
! ----------------------------------------------------------------------
      Enaatm = 0.0_dp
      Enascf = 0.0_dp
!$OMP parallel do default(shared), private(ip),
!$OMP&reduction(+:Enaatm,Enascf)
      do ip = 1, ntpl
        Enaatm   = Enaatm + Vna(ip) * rhoatm(ip)
        Enascf   = Enascf + Vna(ip) * DRho(ip,1)
        Vaux(ip) = Vaux(ip) + Vna(ip)
      enddo
!$OMP end parallel do
      Enaatm = Enaatm * dVol
      Enascf = Enaatm + Enascf * dVol

! ----------------------------------------------------------------------
! Add potential from external electric field (if present)
! ----------------------------------------------------------------------
      if ( acting_efield ) then
         if ( dipole_correction ) then
            field = get_field_from_dipole(dipol,cell)
            if (Node == 0) then
	       write(6,'(a,3f12.4,a)')
     $              'Dipole moment in unit cell   =', dipol/Debye, ' D'
	       write(6,'(a,3f12.6,a)')
     $              'Electric field for dipole correction =',
     $              field/eV*Ang, ' eV/Ang/e'
            end if
            ! The dipole correction energy has an extra factor
            ! of one half because the field involved is internal.
            ! See the paper by Bengtsson DOI:10.1103/PhysRevB.59.12301
            ! Hence we compute this part separately
            DUext = -0.5_dp * ddot(3,field,1,dipol,1)
         else
            field = 0._dp
            DUext = 0._dp
         end if
         ! Add the external electric field
         field = field + user_specified_field
         ! This routine expects a sequential array,
         ! but it is distribution-blind
         call add_potential_from_field( field, cell, nua, isa, xa,
     &                                  ntm, nsm, Vaux )
         ! Add energy of external electric field
         DUext = DUext - ddot(3,user_specified_field,1,dipol,1)
      endif

! ---------------------------------------------------------------------
!     Transiesta:
!     add the potential corresponding to the (possible) voltage-drop.
!     note that ts_voltage is not sharing the reord wih efield since
!     we should not encounter both at the same time.
! ---------------------------------------------------------------------
      if (TSmode.and.IsVolt.and.TSrun) then
         ! This routine expects a sequential array,
         ! in whatever distribution
#ifdef TRANSIESTA_VOLTAGE_DEBUG
!$OMP parallel workshare default(shared)
         Vaux(:) = 0._dp
!$OMP end parallel workshare
#endif
         call ts_voltage(cell, ntm, ntml, Vaux)
#ifdef TRANSIESTA_VOLTAGE_DEBUG
         call write_grid_netcdf( cell, ntm, 1, ntpl, Vaux, 
     &        "TransiestaHartreePotential")
         call timer('ts_volt', 3)
         call bye('transiesta debug for Hartree potential')
#endif
      endif

! ----------------------------------------------------------------------
! Add potential from user defined geometries (if present)
! ----------------------------------------------------------------------
      call hartree_add( cell, ntpl, Vaux )

! ----------------------------------------------------------------------
!     Save electrostatic potential
! ----------------------------------------------------------------------
      if (filesOut%vh .ne. ' ') then
        ! Note that only the first spin component is used
#ifdef NCDF_4
        if ( write_cdf ) then
           call cdf_save_grid(trim(slabel)//'.nc','Vh',1,ntml,
     &          Vaux)
        else
           call write_rho( filesOut%vh, cell, ntm, nsm, ntpl, 1, Vaux )
           call write_grid_netcdf( cell, ntm, 1, ntpl, Vaux, 
     &          "ElectrostaticPotential")
        end if
#else
        call write_rho( filesOut%vh, cell, ntm, nsm, ntpl, 1, Vaux )
        call write_grid_netcdf( cell, ntm, 1, ntpl,
     &       Vaux, "ElectrostaticPotential")
#endif
      endif

!     Get back spin density from sum and difference
      ! TODO Check for NC/SO:
      ! Should we diagonalize locally at every point first?
      if (nsd .eq. 2) then
!$OMP parallel do default(shared), private(ip,rhotot)
        do ip = 1, ntpl
          rhotot     = DRho(ip,1)
          DRho(ip,1) = 0.5_dp * (rhotot - DRho(ip,2))
          DRho(ip,2) = 0.5_dp * (rhotot + DRho(ip,2))
        enddo
!$OMP end parallel do
      endif

! ----------------------------------------------------------------------
#ifndef BSC_CELLXC
! Set uniform distribution of mesh points and find my processor mesh box
! This is the interface to JM Soler's own cellxc routine, which sets
! up the right distribution internally.
! ----------------------------------------------------------------------

      call jms_setMeshDistr( distrID=JDGdistr, nMesh=ntm, 
     .                   nNodesX=1, nNodesY=ProcessorY, nBlock=nsm )
      call myMeshBox( ntm, JDGdistr, myBox )

! ----------------------------------------------------------------------
#endif /* ! BSC_CELLXC */
! Exchange-correlation energy
! ----------------------------------------------------------------------
      call re_alloc( Vscf, 1, ntpl, 1, nspin, 'Vscf', 'dhscf' )

      if (npcc .eq. 1) then
         
!$OMP parallel do default(shared), private(ip,ispin), collapse(2)
       do ispin = 1,nsd
          do ip= 1, ntpl
             DRho(ip,ispin) = DRho(ip,ispin) +
     &            (rhopcc(ip)+rhoatm(ip)) * rnsd
          enddo
       enddo
!$OMP end parallel do

      else

!$OMP parallel do default(shared), private(ip,ispin), collapse(2)
       do ispin = 1,nsd
          do ip= 1, ntpl
             DRho(ip,ispin) = DRho(ip,ispin) +
     &            rhoatm(ip) * rnsd
          enddo
       enddo
!$OMP end parallel do

      end if

      ! Write the electron density used by cellxc
      if (filesOut%rhoxc .ne. ' ') then
#ifdef NCDF_4
        if ( write_cdf ) then
           call cdf_save_grid(trim(slabel)//'.nc','RhoXC',nspin,ntml,
     &          DRho )
        else
           call write_rho( filesOut%rhoxc, cell, ntm, nsm, ntpl, nspin,
     &          DRho )
           call write_grid_netcdf( cell, ntm, nspin, ntpl, DRho, 
     &          "RhoXC")
        end if
#else
        call write_rho( filesOut%rhoxc, cell, ntm, nsm, ntpl, nspin,
     &       DRho )
        call write_grid_netcdf( cell, ntm, nspin, ntpl, DRho, 
     &       "RhoXC")
#endif
      endif

!     Everything now is in UNIFORM, sequential form

      call timer("CellXC",1)
#ifdef BSC_CELLXC
      if (nodes.gt.1) then
         call setMeshDistr( LINEAR, nsm, nsp, nml, nmpl, ntml, ntpl )
      endif

      call re_alloc( Vscf_gga, 1, ntpl, 1, nspin, 'Vscf_gga', 'dhscf' )
      call re_alloc( DRho_gga, 1, ntpl, 1, nspin, 'DRho_gga', 'dhscf' )

      ! Redistribute all spin densities
      do ispin = 1, nspin
        fsrc => DRho(:,ispin)
        fdst => DRho_gga(:,ispin)
        call distMeshData( UNIFORM, fsrc, LINEAR, fdst, KEEP )
      enddo

      call bsc_cellxc( 0, 0, cell, ntml, ntml, ntpl, 0, aux3, nspin,
     &             DRho_gga, Ex, Ec, DEx, DEc, Vscf_gga,
     &             dummy_DVxcdn, stressl )
#endif /* BSC_CELLXC */

#ifndef BSC_CELLXC
      call cellXC( 0, cell, ntm, myBox(1,1), myBox(2,1),
     .                           myBox(1,2), myBox(2,2),
     .                           myBox(1,3), myBox(2,3), nspin,
     .             DRho, Ex, Ec, DEx, DEc, stressXC, Vscf )
#else /* BSC_CELLXC */

      if (nodes.gt.1) then
         call setMeshDistr( UNIFORM, nsm, nsp, nml, nmpl, ntml, ntpl )
      endif

      ! Redistribute to the Vxc array
      do ispin = 1,nspin
        fsrc => Vscf_gga(:,ispin)
        fdst => Vscf(:,ispin)
        call distMeshData( LINEAR, fsrc, UNIFORM, fdst, KEEP )
      enddo
#endif /* BSC_CELLXC */

      if ( debug_dhscf ) then
         write(*,debug_fmt) Node,'XC',
     &        (sqrt(sum(Vscf(:,ispin)**2)),ispin=1,nspin)
      end if
      
      
#ifndef BSC_CELLXC
!     Vscf is still sequential after the call to JMS's cellxc
#else /* BSC_CELLXC */
      call de_alloc( DRho_gga, 'DRho_gga', 'dhscf' )
      call de_alloc( Vscf_gga, 'Vscf_gga', 'dhscf' )
#endif /* BSC_CELLXC */

      Exc =  Ex +  Ec
      Dxc = DEx + DEc

      call timer("CellXC",2)

!     Vscf contains only Vxc, and is UNIFORM and sequential
!     Now we add up the other contributions to it, at 
!     the same time that we get DRho back to true DeltaRho form
!$OMP parallel default(shared), private(ip,ispin)

      ! Hartree potential only has diagonal components
      do ispin = 1,nsd
        if (npcc .eq. 1) then
!$OMP do
           do ip = 1,ntpl
              DRho(ip,ispin) = DRho(ip,ispin) - 
     &             (rhoatm(ip)+rhopcc(ip)) * rnsd
              Vscf(ip,ispin) = Vscf(ip,ispin) + Vaux(ip)
           enddo
!$OMP end do
        else
!$OMP do
           do ip = 1,ntpl
              DRho(ip,ispin) = DRho(ip,ispin) - rhoatm(ip) * rnsd
              Vscf(ip,ispin) = Vscf(ip,ispin) + Vaux(ip)
           enddo
!$OMP end do
        endif
      enddo
!$OMP end parallel

#ifndef BSC_CELLXC
      stress = stress + stressXC
#endif /* ! BSC_CELLXC */

! ----------------------------------------------------------------------
!     Save total potential
! ----------------------------------------------------------------------
      if (filesOut%vt .ne. ' ') then
#ifdef NCDF_4
        if ( write_cdf ) then
           call cdf_save_grid(trim(slabel)//'.nc','Vt',nspin,ntml,
     &          Vscf)
        else 
           call write_rho( filesOut%vt, cell, ntm, nsm, ntpl, nspin,
     &          Vscf )
           call write_grid_netcdf( cell, ntm, nspin, ntpl, Vscf, 
     &          "TotalPotential")
        end if
#else
        call write_rho( filesOut%vt, cell, ntm, nsm, ntpl, nspin, Vscf)
        call write_grid_netcdf( cell, ntm, nspin, ntpl,
     &       Vscf, "TotalPotential")
#endif
      endif

! ----------------------------------------------------------------------
! Print vacuum level
! ----------------------------------------------------------------------

      if (filesOut%vt/=' ' .or. filesOut%vh/=' ') then
        forall(ispin=1:nsd) 
     .    DRho(:,ispin) = DRho(:,ispin) + rhoatm(:) * rnsd
        call vacuum_level( ntpl, nspin, DRho, Vscf, 
     .                     np_vac, Vmax_vac, Vmean_vac )
        forall(ispin=1:nsd) 
     .    DRho(:,ispin) = DRho(:,ispin) - rhoatm(:) * rnsd
        if (np_vac>0 .and. Node==0) print'(/,a,2f12.6,a)', 
     .    'dhscf: Vacuum level (max, mean) =', 
     .    Vmax_vac/eV, Vmean_vac/eV, ' eV'
      endif

      if (filesOut%ebs_dens /= '') then
         call save_ebs_density()
      endif
      
! ----------------------------------------------------------------------
!     Find SCF contribution to hamiltonian matrix elements
! ----------------------------------------------------------------------
      if (iHmat .eq. 1) then
         if (nodes.gt.1) then
            call setMeshDistr( QUADRATIC, nsm, nsp,
     &                         nml, nmpl, ntml, ntpl )
         endif

!        This is a work array, to which we copy Vscf
         call re_alloc( Vscf_par, 1, ntpl, 1, nspin,
     &                 'Vscf_par', 'dhscf' )

         do ispin = 1, nspin
           fsrc => Vscf(:,ispin)
           fdst => Vscf_par(:,ispin)
           call distMeshData( UNIFORM, fsrc,
     &                        QUADRATIC, fdst, TO_CLUSTER )
         enddo

         if (Spiral) then
            call vmatsp( norb, nmpl, dvol, nspin, Vscf_par, maxnd,
     &           numd, listdptr, listd, Hmat, nuo,
     &           nuotot, iaorb, iphorb, isa, qspiral )
         else
            call vmat( norb, nmpl, dvol, spin, Vscf_par, maxnd,
     &           numd, listdptr, listd, Hmat, nuo,
     &           nuotot, iaorb, iphorb, isa )
         endif

         call de_alloc( Vscf_par,  'Vscf_par', 'dhscf' )
         if (nodes.gt.1) then
!          Everything back to UNIFORM, sequential
           call setMeshDistr( UNIFORM, nsm, nsp,
     &                         nml, nmpl, ntml, ntpl )
         endif
      endif


#ifdef MPI
!     Global reduction of Uscf/DUscf/Uatm/Enaatm/Enascf
#ifndef BSC_CELLXC
!     Note that Exc and Dxc are already reduced in the new cellxc
#endif /* ! BSC_CELLXC */
      sbuffer(1) = Uscf
      sbuffer(2) = DUscf
      sbuffer(3) = Uatm
      sbuffer(4) = Enaatm
      sbuffer(5) = Enascf
#ifdef BSC_CELLXC
      sbuffer(6) = Exc
      sbuffer(7) = Dxc
#else
      sbuffer(6:7) = 0._dp
#endif /* BSC_CELLXC */
      call MPI_AllReduce( sbuffer, rbuffer, 7, MPI_double_precision,
     &                     MPI_Sum, MPI_Comm_World, MPIerror )
      Uscf   = rbuffer(1)
      DUscf  = rbuffer(2)
      Uatm   = rbuffer(3)
      Enaatm = rbuffer(4)
      Enascf = rbuffer(5)
#ifdef BSC_CELLXC
      Exc    = rbuffer(6)
      Dxc    = rbuffer(7)
#endif /* BSC_CELLXC */
#endif /* MPI */

!     Add contribution to stress from the derivative of the Jacobian of ---
!     r->r' (strained r) in the integral of Vna*(rhoscf-rhoatm)
      if (istr .eq. 1) then
        do i = 1,3
          stress(i,i) = stress(i,i) + ( Enascf - Enaatm ) / volume
        enddo
      endif

!     Stop time counter for SCF iteration part
      call timer( 'DHSCF3', 2 )

! ----------------------------------------------------------------------
!     End of SCF iteration part
! ----------------------------------------------------------------------

      if (ifa.eq.1 .or. istr.eq.1) then
! ----------------------------------------------------------------------
! Forces and stress : SCF contribution
! ----------------------------------------------------------------------
!       Start time counter for force calculation part
        call timer( 'DHSCF4', 1 )

!       Find contribution of partial-core-correction
        if (npcc .eq. 1) then
          call reord( rhopcc, rhopcc, nml, nsm, TO_CLUSTER )
          call reord( Vaux, Vaux, nml, nsm, TO_CLUSTER )
          ! The partial core calculation only acts on
          ! the diagonal spin-components (no need to
          ! redistribute un-used elements)
          do ispin = 1, nsd
            call reord( Vscf(:,ispin), Vscf(:,ispin),
     &                  nml, nsm, TO_CLUSTER )
          enddo

          call PartialCoreOnMesh( na, isa, ntpl, rhopcc, indxua, nsd,
     &                            dvol, volume, Vscf, Vaux, Fal,
     &                            stressl, ifa.ne.0, istr.ne.0 )
   
          call reord( rhopcc, rhopcc, nml, nsm, TO_SEQUENTIAL )
          call reord( Vaux, Vaux, nml, nsm, TO_SEQUENTIAL )
          ! ** see above
          do ispin = 1, nsd
            call reord( Vscf(:,ispin), Vscf(:,ispin),
     &                  nml, nsm, TO_SEQUENTIAL )
          enddo

          if ( debug_dhscf ) then
             write(*,debug_fmt) Node,'PartialCore',
     &            (sqrt(sum(Vscf(:,ispin)**2)),ispin=1,nsd)
          end if
        endif

        if ( harrisfun) then
!         Forhar deals internally with its own needs
!         for distribution changes
#ifndef BSC_CELLXC
          call forhar( ntpl, nspin, nml, ntml, ntm, npcc, cell, 
#else /* BSC_CELLXC */
          call forhar( ntpl, nspin, nml, ntml, npcc, cell, 
#endif /* BSC_CELLXC */
     &                 rhoatm, rhopcc, Vna, DRho, Vscf, Vaux )
!         Upon return, everything is UNIFORM, sequential form
        endif

!     Transform spin density into sum and difference
        ! TODO NC/SO
        ! Should we perform local diagonalization?
        if (nsd .eq. 2) then
!$OMP parallel do default(shared), private(rhotot,ip)
          do ip = 1,ntpl
            rhotot     = DRho(ip,1) + DRho(ip,2)
            DRho(ip,2) = DRho(ip,2) - DRho(ip,1)
            DRho(ip,1) = rhotot
          enddo
!$OMP end parallel do
        endif

!       Find contribution of neutral-atom potential
        call reord( Vna, Vna, nml, nsm, TO_CLUSTER )
        call reord( DRho, DRho, nml, nsm, TO_CLUSTER )
        call NeutralAtomOnMesh( na, isa, ntpl, Vna, indxua, dvol, 
     &                          volume, DRho, Fal, stressl, 
     &                          ifa.ne.0, istr.ne.0 )
        call reord( DRho, DRho, nml, nsm, TO_SEQUENTIAL )
        call reord( Vna, Vna, nml, nsm, TO_SEQUENTIAL )

        if (nodes.gt.1) then
           call setMeshDistr( QUADRATIC, nsm, nsp,
     &                       nml, nmpl, ntml, ntpl )
        endif

        call re_alloc( Vscf_par, 1, ntpl, 1, nspin,
     &                 'Vscf_par', 'dhscf' )
        do ispin = 1, nspin
          fsrc => Vscf(:,ispin)
          fdst => Vscf_par(:,ispin)
          call distMeshData( UNIFORM, fsrc,
     &                       QUADRATIC, fdst, TO_CLUSTER )
        enddo

!       Remember that Vaux contains everything except Vxc
        call re_alloc( Vaux_par, 1, ntpl, 'Vaux_par', 'dhscf' )
        call distMeshData( UNIFORM, Vaux,
     &                     QUADRATIC, Vaux_par, TO_CLUSTER )

        call dfscf( ifa, istr, na, norb, nuo, nuotot, nmpl, nspin,
     &              indxua, isa, iaorb, iphorb, 
     &              maxnd, numd, listdptr, listd, Dscf, datm,
     &              Vscf_par, Vaux_par, dvol, volume, Fal, stressl )

        call de_alloc( Vaux_par, 'Vaux_par', 'dhscf' )
        call de_alloc( Vscf_par, 'Vscf_par', 'dhscf' )

        if (nodes.gt.1) then
          call setMeshDistr( UNIFORM, nsm, nsp, nml, nmpl, ntml, ntpl )
        endif


!       Stop time counter for force calculation part
        call timer( 'DHSCF4', 2 )
! ----------------------------------------------------------------------
!       End of force and stress calculation
! ----------------------------------------------------------------------
      endif

!     We are in the UNIFORM distribution
!     Rhoatm, Rhopcc and Vna are in UNIFORM dist, sequential form
!     The index array endpht is in the QUADRATIC distribution

!     Stop time counter
      call timer( 'DHSCF', 2 )

! ----------------------------------------------------------------------
!     Free locally allocated memory
! ----------------------------------------------------------------------
      call de_alloc( Vaux, 'Vaux', 'dhscf' )
      call de_alloc( Vscf, 'Vscf', 'dhscf' )
      call de_alloc( DRho, 'DRho', 'dhscf' )

#ifdef DEBUG
      call write_debug( '    POS DHSCF' )
#endif
!------------------------------------------------------------------------ END
      CONTAINS

      subroutine save_bader_charge()
      use meshsubs, only: ModelCoreChargeOnMesh
#ifdef NCDF_4
      use siesta_options, only: write_cdf
      use m_ncdf_siesta, only: cdf_save_grid
#endif
      ! Auxiliary routine to output the Bader Charge
      !
      real(grid_p), pointer :: BaderCharge(:) => null()

      call re_alloc( BaderCharge, 1, ntpl, name='BaderCharge',
     &                 routine='dhscf' )

      ! Find a model core charge by re-scaling the local charge
      call ModelCoreChargeOnMesh( na, isa, ntpl, BaderCharge, indxua )
      ! It comes out in clustered form, so we convert it
      call reord( BaderCharge, BaderCharge, nml, nsm, TO_SEQUENTIAL)
      do ispin = 1,nsd
         BaderCharge(1:ntpl) = BaderCharge(1:ntpl) + DRho(1:ntpl,ispin)
      enddo
#ifdef NCDF_4
      if ( write_cdf ) then
         call cdf_save_grid(trim(slabel)//'.nc','RhoBader',1,ntml,
     &        BaderCharge)
      else
         call write_rho( trim(slabel)// ".BADER", cell,
     $        ntm, nsm, ntpl, 1, BaderCharge )
         call write_grid_netcdf( cell, ntm, 1, ntpl,
     $        BaderCharge, "BaderCharge")
      end if
#else
      call write_rho( trim(slabel)// ".BADER", cell,
     $     ntm, nsm, ntpl, 1, BaderCharge )
      call write_grid_netcdf( cell, ntm, 1, ntpl,
     $     BaderCharge, "BaderCharge")
#endif

      call de_alloc( BaderCharge, name='BaderCharge' )
      end subroutine save_bader_charge

      subroutine setup_analysis_options()
      !! For the analyze-charge-density-only case,
      !! avoiding any diagonalization

      use siesta_options, only: hirshpop, voropop
      use siesta_options, only: saverho, savedrho, saverhoxc
      use siesta_options, only: savevh, savevt, savevna
      use siesta_options, only: savepsch, savetoch

      want_partial_charges = (hirshpop .or. voropop)

      if (saverho)   filesOut%rho   = trim(slabel)//'.RHO'
      if (savedrho)  filesOut%drho  = trim(slabel)//'.DRHO'
      if (saverhoxc) filesOut%rhoxc = trim(slabel)//'.RHOXC'
      if (savevh)    filesOut%vh    = trim(slabel)//'.VH'
      if (savevt)    filesOut%vt    = trim(slabel)//'.VT'
      if (savevna)   filesOut%vna   = trim(slabel)//'.VNA'
      if (savepsch)  filesOut%psch  = trim(slabel)//'.IOCH'
      if (savetoch)  filesOut%toch  = trim(slabel)//'.TOCH'

      end subroutine setup_analysis_options

      subroutine save_ebs_density()
!! Optional output of the "band-structure energy density", which
!! is just the charge density weighted by the eigenvalues, i.e.,
!! using EDM instead of DM in rhoofd

      use sparse_matrices, only: Escf

      real(grid_p), pointer :: Ebs_dens(:,:) => null(),
     &                         Ebs_dens_quad(:,:) => null()

!     Allocate memory for Ebs_dens using the UNIFORM data distribution
      call re_alloc( Ebs_dens, 1, ntpl, 1, nspin, 'Ebs_dens','dhscf')

!     Switch to quadratic distribution for call to rhoofd
      if (nodes.gt.1) then
         call setMeshDistr( QUADRATIC, nsm, nsp,
     &        nml, nmpl, ntml, ntpl )
      endif

      call re_alloc( ebs_dens_quad, 1, ntpl, 1, nspin,
     &     'Ebs_dens_quad', 'dhscf' )
      call rhoofd( norb, nmpl, maxnd, numd, listdptr, listd,
     &     nspin, Escf, Ebs_dens_quad,
     &     nuo, nuotot, iaorb, iphorb, isa )

!     Ebs_dens_par is here in QUADRATIC, clustered form

!     Set the UNIFORM distribution again and copy Ebs_dens to it
      if (nodes.gt.1) then
         call setMeshDistr( UNIFORM, nsm, nsp, nml, nmpl, ntml, ntpl )
      endif
      do ispin = 1, nspin
         fsrc => Ebs_dens_quad(:,ispin)
         fdst => Ebs_dens(:,ispin)
         ! Sequential to be able to write it out
         ! if nodes==1, this call will just reorder
         call distMeshData( QUADRATIC, fsrc,
     &        UNIFORM, fdst, TO_SEQUENTIAL )
      enddo
      call de_alloc( Ebs_dens_quad, 'Ebs_dens_quad','dhscf' )
#ifdef NCDF_4
      if ( write_cdf ) then
         call cdf_save_grid(trim(slabel)//'.nc','Ebs_density',
     $        nspin,ntml, Ebs_dens)
      else
         call write_rho( filesOut%ebs_dens,
     $        cell, ntm, nsm, ntpl, nspin, Ebs_dens )
         call write_grid_netcdf( cell, ntm, nspin, ntpl, Ebs_dens,
     &        "Ebs_density")
      end if
#else
      call write_rho( filesOut%ebs_dens, cell, ntm, nsm, ntpl, nspin,
     $     Ebs_dens)
      call write_grid_netcdf( cell, ntm, nspin, ntpl,
     &     Ebs_dens, "Ebs_density")
#endif
      call de_alloc( Ebs_dens, 'Ebs_dens','dhscf' )

      end subroutine save_ebs_density

      end subroutine dhscf

      subroutine delk_wrapper(isigneikr, norb, maxnd,
     &                        numd, listdptr, listd,
     &                        nuo,  nuotot, iaorb, iphorb, isa )
!! This is a wrapper to call [[delk(proc)]], using some of the module
!! variables of `m_dhscf`, but from outside `dhscf` itself.
!!
!! The dhscf module variables used are:
!!
!! * nmpl
!! * dvol
!! * nml
!! * nmpl
!! * ntml
!! * ntpl
!!
!! Some of them might be put somewhere else (mesh?) to allow some
!! of the kitchen-sink functionality of dhscf to be made more modular.
!! For example, this wrapper might live independently if enough mesh
!! information is made available to it.

      use m_delk,  only  : delk  ! The real workhorse, similar to vmat

      use moreMeshSubs,   only : setMeshDistr
      use moreMeshSubs,   only : UNIFORM, QUADRATIC
      use parallel,       only : Nodes
      use mesh,           only : nsm, nsp

      integer                  :: isigneikr,
     &                            norb, nuo, nuotot, maxnd,
     &                            iaorb(*), iphorb(*), isa(*),
     &                            numd(nuo),
     &                            listdptr(nuo), listd(maxnd)


! ----------------------------------------------------------------------
! Calculate matrix elements of exp(i \vec{k} \cdot \vec{r})
! ----------------------------------------------------------------------
        if (isigneikr .eq. 1 .or. isigneikr .eq. -1) then

          if (nodes.gt.1) then
            call setMeshDistr( QUADRATIC, nsm, nsp,
     &                         nml, nmpl, ntml, ntpl )
          endif

          call delk( isigneikr, norb, nmpl, dvol, maxnd,
     &               numd, listdptr, listd,
     &               nuo,  nuotot, iaorb, iphorb, isa )

          if (nodes.gt.1) then
!           Everything back to UNIFORM, sequential
            call setMeshDistr( UNIFORM, nsm, nsp,
     &                         nml, nmpl, ntml, ntpl )
          endif

        endif
      end subroutine delk_wrapper

      end module m_dhscf
