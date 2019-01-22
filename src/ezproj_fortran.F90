!------------------------------GPL---------------------------------------//
! This file is part of EZPROJ.
!
! (c) 2015-2018 Zachary Cobell
!
! EZPROJ is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! EZPROJ is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with EZPROJ.  If not, see <http://www.gnu.org/licenses/>.
!------------------------------------------------------------------------//
       MODULE EZPROJMODULE
           USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_PTR
           IMPLICIT NONE
           
           TYPE EZPROJ
               PRIVATE
               LOGICAL     :: initialized = .FALSE.
               TYPE(C_PTR) :: ptr
           END TYPE EZPROJ

           INTERFACE 
               TYPE(C_PTR) FUNCTION c_ezproj_create() BIND(C,NAME="ezproj_create_") RESULT(ptr)
                   USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_PTR
                   IMPLICIT NONE
               END FUNCTION c_ezproj_create
               SUBROUTINE c_ezproj_delete(transformer) BIND(C,NAME="ezproj_delete_")
                   USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_INT,C_DOUBLE,C_PTR
                   IMPLICIT NONE
                   TYPE(C_PTR),VALUE,INTENT(IN)  :: transformer
               END SUBROUTINE c_ezproj_delete
               SUBROUTINE c_ezproj_project(transformer,epsg_from,epsg_to,x,y,out_x,out_y,ierr) BIND(C,NAME="ezproj_project_")
                   USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_INT,C_DOUBLE,C_PTR
                   IMPLICIT NONE
                   TYPE(C_PTR),VALUE,INTENT(IN) :: transformer
                   INTEGER(C_INT),INTENT(IN)    :: EPSG_FROM,EPSG_TO
                   INTEGER(C_INT),INTENT(OUT)   :: ierr
                   REAL(C_DOUBLE),INTENT(IN)    :: x,y
                   REAL(C_DOUBLE),INTENT(OUT)   :: out_x,out_y
               END SUBROUTINE c_ezproj_project
               SUBROUTINE c_ezproj_cpp(lambda0,phi0,x,y,out_x,out_y,ierr) BIND(C,NAME="ezproj_cpp_")
                   USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_INT,C_DOUBLE
                   IMPLICIT NONE
                   REAL(C_DOUBLE),INTENT(IN)    :: lambda0,phi0,x,y
                   REAL(C_DOUBLE),INTENT(OUT)   :: out_x,out_y
                   INTEGER(C_INT),INTENT(OUT)   :: ierr
               END SUBROUTINE c_ezproj_cpp
               SUBROUTINE c_ezproj_inverseCpp(lambda0,phi0,x,y,out_x,out_y,ierr) BIND(C,NAME="ezproj_inverseCpp_")
                   USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_INT,C_DOUBLE
                   IMPLICIT NONE
                   REAL(C_DOUBLE),INTENT(IN)    :: lambda0,phi0,x,y
                   REAL(C_DOUBLE),INTENT(OUT)   :: out_x,out_y
                   INTEGER(C_INT),INTENT(OUT)   :: ierr
               END SUBROUTINE c_ezproj_inverseCpp
           END INTERFACE

           CONTAINS

           SUBROUTINE ezproj_init(pj)
               USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_PTR
               IMPLICIT NONE
               TYPE(EZPROJ),INTENT(OUT)    :: pj
               pj%ptr = c_ezproj_create()
               pj%initialized = .TRUE.
               RETURN
           END SUBROUTINE ezproj_init


           SUBROUTINE ezproj_project(pj,epsg_from,epsg_to,x,y,out_x,out_y,ierr)
               USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_INT,C_DOUBLE,C_PTR
               IMPLICIT NONE
               TYPE(EZPROJ),INTENT(INOUT)   :: pj
               INTEGER,INTENT(IN)           :: epsg_from
               INTEGER,INTENT(IN)           :: epsg_to
               REAL(8),INTENT(IN)           :: x
               REAL(8),INTENT(IN)           :: y
               REAL(8),INTENT(OUT)          :: out_x
               REAL(8),INTENT(OUT)          :: out_y
               INTEGER,INTENT(OUT)          :: ierr

               REAL(C_DOUBLE)               :: c_x
               REAL(C_DOUBLE)               :: c_y
               REAL(C_DOUBLE)               :: c_xout
               REAL(C_DOUBLE)               :: c_yout
               INTEGER(C_INT)               :: c_epsg_from
               INTEGER(C_INT)               :: c_epsg_to
               INTEGER(C_INT)               :: c_ierr

               c_x         = REAL(x,C_DOUBLE)
               c_y         = REAL(y,C_DOUBLE)
               c_epsg_from = INT(epsg_from,C_INT)
               c_epsg_to   = INT(epsg_to,C_INT)

               IF(.NOT.pj%initialized)THEN
                   CALL ezproj_init(pj)
               ENDIF

               CALL c_ezproj_project(pj%ptr,c_epsg_from,c_epsg_to,c_x,c_y,c_xout,c_yout,c_ierr)
       
               out_x       = REAL(c_xout,8)
               out_y       = REAL(c_yout,8)
               ierr        = INT(c_ierr,4)

               RETURN

           END SUBROUTINE ezproj_project


           SUBROUTINE ezproj_delete(pj)
               USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_PTR
               IMPLICIT NONE
               TYPE(EZPROJ),INTENT(INOUT) :: pj
       
               IF(pj%initialized)THEN
                   CALL c_ezproj_delete(pj%ptr)
                   pj%initialized = .FALSE.
               ENDIF

               RETURN

           END SUBROUTINE ezproj_delete

           SUBROUTINE ezproj_cpp(lambda0,phi0,x,y,out_x,out_y,ierr)
               USE,INTRINSIC :: ISO_C_BINDING,ONLY: C_INT,C_DOUBLE
               IMPLICIT NONE
               REAL(8),INTENT(IN)  :: lambda0, phi0, x, y
               REAL(8),INTENT(OUT) :: out_x, out_y
               INTEGER,INTENT(OUT) :: ierr

               REAL(C_DOUBLE)      :: c_x,c_y,c_xout,c_yout,c_lambda0,c_phi0
               INTEGER(C_INT)      :: c_ierr

               c_x = REAL(x,C_DOUBLE)
               c_y = REAL(y,c_DOUBLE)
               c_lambda0 = REAL(lambda0,C_DOUBLE)
               c_phi0 = REAL(phi0,C_DOUBLE)

               CALL c_ezproj_cpp(c_lambda0,c_phi0,c_x,c_y,c_xout,c_yout,c_ierr)

               out_x = REAL(c_xout,C_DOUBLE)
               out_y = REAL(c_yout,C_DOUBLE)
           END SUBROUTINE ezproj_cpp
           
           SUBROUTINE ezproj_inversecpp(lambda0,phi0,x,y,out_x,out_y,ierr)
               USE,INTRINSIC :: ISO_C_BINDING,ONLY: C_INT,C_DOUBLE
               IMPLICIT NONE
               REAL(8),INTENT(IN)  :: lambda0, phi0, x, y
               REAL(8),INTENT(OUT) :: out_x, out_y
               INTEGER,INTENT(OUT) :: ierr

               REAL(C_DOUBLE)      :: c_x,c_y,c_xout,c_yout,c_lambda0,c_phi0
               INTEGER(C_INT)      :: c_ierr

               c_x = REAL(x,C_DOUBLE)
               c_y = REAL(y,c_DOUBLE)
               c_lambda0 = REAL(lambda0,C_DOUBLE)
               c_phi0 = REAL(phi0,C_DOUBLE)

               CALL c_ezproj_inversecpp(c_lambda0,c_phi0,c_x,c_y,c_xout,c_yout,c_ierr)

               out_x = REAL(c_xout,C_DOUBLE)
               out_y = REAL(c_yout,C_DOUBLE)
           END SUBROUTINE ezproj_inversecpp


       END MODULE EZPROJMODULE
