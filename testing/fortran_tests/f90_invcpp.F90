

        PROGRAM TEST_EZPROJ_CPP
            USE EZPROJMODULE
            IMPLICIT NONE

            REAL(8),PARAMETER :: x_original = -35625.789763D0
            REAL(8),PARAMETER :: y_original = 3343350.030257D0
            REAL(8),PARAMETER :: x_expected = -90.3661965220D0
            REAL(8),PARAMETER :: y_expected = 30.0573591369D0
            REAL(8),PARAMETER :: lambda0 = -90.0D0
            REAL(8),PARAMETER :: phi0 = 29.0D0
            REAL(8),PARAMETER :: tol = 0.000001D0
            REAL(8)           :: x_out,y_out
            INTEGER           :: ierr

            CALL ezproj_inversecpp(lambda0,phi0,x_original,y_original,x_out,y_out,ierr)

            IF(ABS(x_out-x_expected).GT.tol.OR.ABS(y_out-y_expected).GT.tol)THEN
                WRITE(*,'(A,F0.6,2X,F0.6)') "Expected: ",x_expected,y_expected
                WRITE(*,'(A,F0.6,2X,F0.6)') "Got: ",x_out,y_out
                WRITE(*,'(A)') "Transformation did not match expected value"
                STOP 1
            ENDIF

            RETURN

        END PROGRAM TEST_EZPROJ_CPP
