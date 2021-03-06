

        PROGRAM TEST_EZPROJ_TOGEOGRAPHIC
            USE EZPROJMODULE
            IMPLICIT NONE

            REAL(8),PARAMETER :: x_original = 753922.922116D0
            REAL(8),PARAMETER :: y_original = 3328065.712818D0
            REAL(8),PARAMETER :: x_expected = -90.3661965220D0
            REAL(8),PARAMETER :: y_expected = 30.0573591369D0
            REAL(8),PARAMETER :: tol = 0.000001D0
            REAL(8)           :: x_out,y_out
            INTEGER           :: ierr
            INTEGER,PARAMETER :: cs1 = 26915
            INTEGER,PARAMETER :: cs2 = 4326
            TYPE(EZPROJ)      :: pj

            CALL ezproj_init(pj)
            CALL ezproj_project(pj,cs1,cs2,x_original,y_original,x_out,y_out,ierr)
            CALL ezproj_delete(pj)

            IF(ABS(x_out-x_expected).GT.tol.OR.ABS(y_out-y_expected).GT.tol)THEN
                WRITE(*,'(A,F0.6,2X,F0.6)') "Expected: ",x_expected,y_expected
                WRITE(*,'(A,F0.6,2X,F0.6)') "Got: ",x_out,y_out
                WRITE(*,'(A)') "Transformation did not match expected value"
                STOP 1
            ENDIF

            RETURN

        END PROGRAM TEST_EZPROJ_TOGEOGRAPHIC
