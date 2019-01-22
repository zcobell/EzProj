

        PROGRAM TEST_EZPROJ_TOUTM15
            USE EZPROJMODULE
            IMPLICIT NONE

            REAL(8),PARAMETER :: x_original = -90.3661965220
            REAL(8),PARAMETER :: y_original = 30.0573591369
            REAL(8),PARAMETER :: x_expected = 753922.922116
            REAL(8),PARAMETER :: y_expected = 3328065.712818
            REAL(8),PARAMETER :: tol = 0.000001
            REAL(8)           :: x_out,y_out
            INTEGER           :: ierr
            INTEGER,PARAMETER :: cs1 = 4326
            INTEGER,PARAMETER :: cs2 = 26915
            TYPE(EZPROJ)      :: pj

            CALL ezproj_init(pj)
            CALL ezproj_project(pj,cs1,cs2,x_original,y_original,x_out,y_out,ierr)
            CALL ezproj_delete(pj)

            IF(ABS(x_out-x_expected).GT.tol.OR.ABS(y_out-y_expected).GT.tol)THEN
                STOP "Transformation did not match expected value"
            ENDIF

            RETURN

        END PROGRAM TEST_EZPROJ_TOUTM15
