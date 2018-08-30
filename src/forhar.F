! 
! Copyright (C) 1996-2016	The SIESTA group
!  This file is distributed under the terms of the
!  GNU General Public License: see COPYING in the top directory
!  or http://www.gnu.org/copyleft/gpl.txt.
! See Docs/Contributors.txt for a list of contributors.
!
      module m_forhar

      use precision,    only : dp, grid_p
      use alloc,        only : re_alloc, de_alloc
#ifndef BSC_CELLXC
      use parallel,  only : ProcessorY
      use mesh,      only : NSM
      use siestaXC,  only : cellXC       ! Finds xc energy and potential
      use siestaXC,  only : myMeshBox    ! Returns my processor mesh box
      use siestaXC, only : jms_setMeshDistr => setMeshDistr
                                        ! Sets a distribution of mesh
                                        ! points over parallel processors

#else /* BSC_CELLXC */
      use mesh
      use moreMeshSubs, only : setMeshDistr, distMeshData
      use moreMeshSubs, only: UNIFORM, LINEAR
      use moreMeshSubs, only: KEEP
#endif /* BSC_CELLXC */
      implicit none

      public :: forhar
      private

      CONTAINS
#ifndef BSC_CELLXC
      subroutine forhar( NTPL, NSPIN, NML, NTML, NTM, NPCC,
     $                   CELL, RHOATM,
#else /* BSC_CELLXC */
      subroutine forhar( NTPL, NSPIN, NML, NTML, NPCC, CELL, RHOATM,
#endif /* BSC_CELLXC */
     &                   RHOPCC, VNA, DRHOOUT, VHARRIS1, VHARRIS2 )
!! author: J.Junquera
!! date: 09/00
!!
!! Build the potentials needed for computing Harris forces:
!!
!!@todo
!! Formulas in description
!!@endtodo
!!
!!\(
!! (V_{NA} + V_{Hartree}(DeltaRho_{in}) - DV_{xc}(Rho_{in})/Dn * (Rho_{out}-Rho_{in}))
!!\)
!!
!!\(
!! (V_{NA} + V_{Hartree}(DeltaRho_in) + V_{xc}(Rho_{in})
!!\)
!!
!! In the first SCF step, \( V_{Hartree}(DeltaRho_{in}) \) is zero, because
!! in that case, \( Rho_{SCF}(r) = Rho_{atm}(r) \) and therefore, \( DeltaRho(r) = 0 \)
!! This calculation will be skipped.
!!
!! If Harris + Spin polarized in the first SCF step, then Vharris2 will
!! multiply to \( D Rho(Harris)/D R \) inside dfscf, and the change of
!! the harris density respect the displacement
!! on one atom will not depend on spin. We add in Vharris2 the
!! contributions of both spins.

      INTEGER             :: NTPL
        !! Number of Mesh Total Points in unit cell (including subpoints) locally.
      INTEGER             :: NML(3), NTML(3)
#ifndef BSC_CELLXC
      INTEGER, intent(IN) :: NTM(3)
        !! Number of mesh divisions of each cell
        !! vector, including subgrid
      INTEGER, INTENT(IN) :: NSPIN
        !! Spin polarizations
      INTEGER, INTENT(IN) :: NPCC
        !! Partial core corrections? (`0`=no,`1`=yes)
#else /* BSC_CELLXC */
      INTEGER, INTENT(IN) :: NSPIN, NPCC
#endif /* BSC_CELLXC */

      REAL(dp),                INTENT(IN) :: CELL(3,3)
        !! Cell vectors
      REAL(grid_p),            INTENT(IN) :: VNA(NTPL)
        !! Sum of neutral atoms potentials
      REAL(grid_p),            INTENT(IN) :: RHOATM(NTPL)
        !! Harris density at mesh points
      REAL(grid_p),            INTENT(IN) :: RHOPCC(NTPL)
        !! Partial-core-correction density for xc
      REAL(grid_p),         INTENT(INOUT) :: DRHOOUT(NTPL,NSPIN)
!! Charge density at the mesh points in current step.
!! The charge density that enters in forhar is \( Drho_{out} - Rho_{atm} \).
      REAL(grid_p), TARGET, INTENT(INOUT) :: VHARRIS1(NTPL,NSPIN)
!! \( V_{na} + V_{Hartree}(DeltaRho_{in}) + V_{xc}(Rho_{in}) \)
      real(grid_p),         intent(INOUT) :: VHARRIS2(NTPL)
!! \(
!! V_{na} + V_{Hartree}(Rho_{in}) +
!! DV_{xc}(Rho_{in})/DRho_{in} * (Rho_{out}-Rho_{in})
!! \)
!!
!! If Harris forces are computed in the first SCF step,
!! it does not depend on spin.

#ifndef BSC_CELLXC
      EXTERNAL REORD
#else /* BSC_CELLXC */
      EXTERNAL bsc_cellxc
#endif /* BSC_CELLXC */

! AG: Note:  REAL*4 variables are really REAL(kind=grid_p)
!
C ***** INTERNAL VARIABLES *********************************************
C REAL*4 DVXDN(NTPL,NSPIN,NSPIN): Derivative of exchange-correlation
C                                potential respect the charge density
C **********************************************************************

C ----------------------------------------------------------------------
C Internal variables and arrays
C ----------------------------------------------------------------------
#ifndef BSC_CELLXC
      INTEGER IP, ISPIN, ISPIN2, myBox(2,3)
      REAL(dp) EX, EC, DEX, DEC, STRESS(3,3)
      INTEGER, SAVE:: JDGdistr=-1

      real(grid_p),  pointer :: drhoin(:,:), 
     &                          dvxcdn(:,:,:) 
#else /* BSC_CELLXC */
      INTEGER                :: IP, ISPIN, ISPIN2, NMPL
      REAL(dp)               :: EX, EC, DEX, DEC, STRESSL(3,3)
      real(grid_p)           :: aux3(3,1)   !! dummy arrays for cellxc

      real(grid_p),  pointer :: drhoin(:,:), drhoin_par(:,:),
     &                          dvxcdn(:,:,:), dvxcdn_par(:,:,:),
     &                          vharris1_par(:,:), fsrc(:), fdst(:)
      INTEGER                :: ntpl_3
#endif /* BSC_CELLXC */

      nullify( drhoin, dvxcdn )
      call re_alloc( drhoin, 1, ntpl, 1, nspin, 'drhoin', 'forhar' )
      call re_alloc( dvxcdn, 1, ntpl, 1, nspin, 1, nspin,
     &               'dvxcdn', 'forhar' )

C ----------------------------------------------------------------------
C Initialize some variables
C ----------------------------------------------------------------------
      VHARRIS1(:,:) = 0.0_grid_p
      VHARRIS2(:)   = 0.0_grid_p
      DRHOIN(:,:)   = 0.0_grid_p
      DVXCDN(:,:,:) = 0.0_grid_p
#ifndef BSC_CELLXC

C ----------------------------------------------------------------------
C Set uniform distribution of mesh points and find my processor mesh box
C ----------------------------------------------------------------------

      call jms_setMeshDistr( distrID=JDGdistr, nMesh=NTM, 
     .                       nNodesX=1, nNodesY=ProcessorY, nBlock=NSM )
      call myMeshBox( NTM, JDGdistr, myBox )
#else /* BSC_CELLXC */
      STRESSL(:,:)  = 0.0_dp
#endif /* BSC_CELLXC */

C ----------------------------------------------------------------------
C Compute exchange-correlation energy and potential and
C their derivatives respect the input charge, that is, Harris charge
C or the sum of atomic charges.
C ----------------------------------------------------------------------

      DO ISPIN = 1, NSPIN
        DRHOIN(1:NTPL,ISPIN) =  RHOATM(1:NTPL)/NSPIN 
        IF (NPCC .EQ. 1) 
     .    DRHOIN(1:NTPL,ISPIN) = DRHOIN(1:NTPL,ISPIN) + 
     .                           RHOPCC(1:NTPL)/NSPIN
      ENDDO

#ifdef BSC_CELLXC
#ifdef MPI
      ! The input distribution is UNIFORM, but we need to work
      ! with the LINEAR one
      call setMeshDistr( LINEAR, nsm, nsp, nml, nmpl, ntml, ntpl )
#endif
      ntpl_3 = ntpl

      nullify( drhoin_par, vharris1_par, dvxcdn_par )
      call re_alloc( drhoin_par, 1, ntpl_3, 1, nspin,
     &               'drhoin_par', 'forhar' )
      call re_alloc( vharris1_par, 1, ntpl_3, 1, nspin,
     &               'vharris1_par', 'forhar' )
      call re_alloc( dvxcdn_par, 1, ntpl_3, 1, nspin, 1, nspin,
     &               'dvxcdn_par', 'forhar' )
#endif /* BSC_CELLXC */

      DO ISPIN = 1, NSPIN
#ifndef BSC_CELLXC
        CALL REORD(DRHOIN(1,ISPIN),DRHOIN(1,ISPIN),NML,NSM,+1)
#else /* BSC_CELLXC */
        fsrc => drhoin(:,ispin)
        fdst => drhoin_par(:,ispin)
        call distMeshData( UNIFORM, fsrc, LINEAR, fdst, KEEP )
#endif /* BSC_CELLXC */
      ENDDO

#ifndef BSC_CELLXC
      CALL CELLXC( 0, CELL, NTM, myBox(1,1), myBox(2,1),
     .                           myBox(1,2), myBox(2,2),
     .                           myBox(1,3), myBox(2,3), NSPIN, DRHOIN,
     .             EX, EC, DEX, DEC, STRESS, VHARRIS1, DVXCDN )
#else /* BSC_CELLXC */
      CALL bsc_cellxc( 0, 1, CELL, NTML, NTML, NTPL, 0, AUX3, NSPIN,
     &             DRHOIN_PAR, EX, EC, DEX, DEC, VHARRIS1_PAR,
     &             DVXCDN_PAR, STRESSL )
#endif /* BSC_CELLXC */

      DO ISPIN = 1, NSPIN
#ifndef BSC_CELLXC
        CALL REORD(DRHOIN(1,ISPIN),DRHOIN(1,ISPIN),NML,NSM,-1)
        CALL REORD(VHARRIS1(1,ISPIN),VHARRIS1(1,ISPIN),NML,NSM,-1)

#else /* BSC_CELLXC */
        fsrc => VHARRIS1_PAR(:,ISPIN)
        fdst => VHARRIS1(:,ispin)
        call distMeshData( LINEAR, fsrc, UNIFORM, fdst, KEEP )
#endif /* BSC_CELLXC */
        DO ISPIN2 = 1, NSPIN
#ifndef BSC_CELLXC
          CALL REORD(DVXCDN(1,ISPIN,ISPIN2),DVXCDN(1,ISPIN,ISPIN2),
     .               NML,NSM,-1)
#else /* BSC_CELLXC */
          fsrc => DVXCDN_PAR(:,ISPIN,ISPIN2)
          fdst => DVXCDN(:,ISPIN,ISPIN2)
          call distMeshData( LINEAR, fsrc, UNIFORM, fdst, KEEP )
#endif /* BSC_CELLXC */
        ENDDO
      ENDDO

#ifdef BSC_CELLXC
      call de_alloc( dvxcdn_par,   'dvxcdn_par',   'forhar' )
      call de_alloc( vharris1_par, 'vharris1_par', 'forhar' )
      call de_alloc( drhoin_par,   'drhoin_par',   'forhar' )

#ifdef MPI
      call setMeshDistr( UNIFORM, nsm, nsp, nml, nmpl, ntml, ntpl )
#endif
#endif /* BSC_CELLXC */
      DO ISPIN = 1, NSPIN
        IF( NPCC .EQ. 1) 
     &    DRHOIN(1:NTPL,ISPIN) = DRHOIN(1:NTPL,ISPIN) - 
     &                           RHOPCC(1:NTPL)/NSPIN
        DO IP = 1, NTPL
          VHARRIS1(IP,ISPIN) = VHARRIS1(IP,ISPIN) + VNA(IP)
        ENDDO
      ENDDO

C ----------------------------------------------------------------------
C Compute the product DV_xc(Rho_in)/DRho_in * (Rho_out - Rho_in).
C Since the charge that enters into forhar is DRHOOUT = Rho_out-Rhoatm
C no extra transformation on the charge density is needed.
C ----------------------------------------------------------------------

      DO ISPIN = 1, NSPIN
        DO ISPIN2 = 1, NSPIN
          DO IP = 1, NTPL
            VHARRIS2(IP) = VHARRIS2(IP) + 
     &                     DVXCDN(IP,ISPIN2,ISPIN) * DRHOOUT(IP,ISPIN2)
          ENDDO
        ENDDO
      ENDDO

C ----------------------------------------------------------------------
C Since V_Hartree(DeltaRho_in) = 0.0, we only add to vharris2 the neutral
C atom potential
C ----------------------------------------------------------------------
      DO IP = 1, NTPL
        VHARRIS2(IP) = VNA(IP) - VHARRIS2(IP)
      ENDDO

      call de_alloc( dvxcdn, 'dvxcdn', 'forhar')
      call de_alloc( drhoin, 'drhoin', 'forhar')
      end subroutine forhar

      end module m_forhar
