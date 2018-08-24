!
! Copyright (C) 1996-2016	The SIESTA group
!  This file is distributed under the terms of the
!  GNU General Public License: see COPYING in the top directory
!  or http://www.gnu.org/copyleft/gpl.txt.
! See Docs/Contributors.txt for a list of contributors.
!
      subroutine reord( fclust, fseq, nm, nsm, itr )
      !! author: J.M.Soler
      !! date: May 1995

!! Re-orders a clustered data array into a sequential one and viceversa

      use precision, only: grid_p
      use alloc,     only: re_alloc, de_alloc

      IMPLICIT NONE
      INTEGER, intent(in)         ::   NM(3)
      !! Number of Mesh divisions in each cell vector
      INTEGER, intent(in)         ::   NSM
      !! Number of Sub-divisions in each Mesh division
      INTEGER, intent(in)         ::   ITR
      !! TRanslation-direction switch  
      !! ITR=+1 => From clustered to sequential  
      !! ITR=-1 => From sequential to clustered

      REAL(grid_p), intent(inout) ::   FCLUST(*)
      !! CLUSTered data:

      !!    REAL*4 FCLUST(NSM,NSM,NSM,NM1,NM2,NM3)
      REAL(grid_p), intent(inout) ::   FSEQ(*)
      !! SEQuential data:
      !!
      !!    REAL*4 FSEQ(NSM*NM1,NSM*NM2,NSM*NM3)

      INTEGER 
     .  I, I0, I1, I2, I3, IS, IS1, IS2, IS3,
     .  J, J0, NSM3, NTM(3), NAUX

      real(grid_p), dimension(:), pointer :: AUX
      integer, dimension(:), pointer :: JS

      CALL TIMER('REORD',1)

      NTM(1) = NM(1) * NSM
      NTM(2) = NM(2) * NSM
      NTM(3) = NM(3) * NSM
      NSM3 = NSM**3
      NAUX = NM(1) * NM(2) * NSM3
!
!  Allocate local memory
!
      nullify ( AUX )
      call re_alloc( AUX, 1, NAUX, 'AUX', 'reord' )
      nullify ( JS )
      call re_alloc( JS,  1, NSM3, 'JS',  'reord' )

      IS = 0
      DO IS3 = 0,NSM-1
        DO IS2 = 0,NSM-1
          DO IS1 = 0,NSM-1
            IS = IS + 1
            JS(IS) = 1 + IS1 + NTM(1)*IS2 + NTM(1)*NTM(2)*IS3
          ENDDO
        ENDDO
      ENDDO

      IF (ITR .GT. 0) THEN
        DO I3 = 0, NM(3)-1
          DO I2 = 0, NM(2)-1
            I0 = NSM3 * ( NM(1)*I2 + NM(1)*NM(2)*I3 )
            J0 = NTM(1)*NSM*I2
            DO IS = 1,NSM3
              I = I0 + IS
              J = J0 + JS(IS)
              DO I1 = 1,NM(1)
                AUX(J) = FCLUST(I)
                I = I + NSM3
                J = J + NSM
              ENDDO
            ENDDO
          ENDDO
          I = NM(1) * NM(2) * NSM3 * I3
          DO J = 1,NM(1)*NM(2)*NSM3
            FSEQ(I+J) = AUX(J)
          ENDDO
        ENDDO
      ELSE
        DO I3 = 0, NM(3)-1
          I = NM(1) * NM(2) * NSM3 * I3
          DO J = 1,NM(1)*NM(2)*NSM3
            AUX(J) = FSEQ(I+J)
          ENDDO
          DO I2 = 0, NM(2)-1
            I0 = NSM3 * ( NM(1)*I2 + NM(1)*NM(2)*I3 )
            J0 = NTM(1)*NSM*I2
            DO IS = 1,NSM3
              I = I0 + IS
              J = J0 + JS(IS)
              DO I1 = 1,NM(1)
                FCLUST(I) = AUX(J)
                I = I + NSM3
                J = J + NSM
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDIF

!
!  Free local memory
!
      call de_alloc( JS,  'JS',   'reord' )
      call de_alloc( AUX, 'AUX',  'reord' )

      CALL TIMER('REORD',2)
      END

