
MODULE linalg
   USE, INTRINSIC :: iso_c_binding
   USE :: fast_math
   IMPLICIT NONE

CONTAINS

   SUBROUTINE f_cross_product(dst, va, vb) BIND (C)
      REAL(C_FLOAT), DIMENSION(3) :: dst, va, vb
      dst(1) = (va(2) * vb(3)) - (vb(2) * va(3))
      dst(2) = (va(3) * vb(1)) - (vb(3) * va(1))
      dst(3) = (va(1) * vb(2)) - (vb(1) * va(2))
   END SUBROUTINE f_cross_product

   SUBROUTINE f_dot_product(dst, va, vb) BIND (C)
      REAL(C_FLOAT), DIMENSION(3) :: va, vb
      REAL(C_FLOAT) :: dst
      dst = DOT_PRODUCT(va, vb)
   END SUBROUTINE f_dot_product

   SUBROUTINE f_wedge_product(dst, va, vb) BIND (C)
      REAL(C_FLOAT), DIMENSION(3) :: dst, va, vb
      dst(1) = (va(1) * vb(2)) - (va(2) * vb(1))
      dst(2) = (va(1) * vb(3)) - (va(3) * vb(1))
      dst(3) = (va(2) * vb(3)) - (va(3) * vb(2))
   END SUBROUTINE f_wedge_product

   SUBROUTINE f_geometric_product(dst, va, vb) BIND(C)
      REAL(C_FLOAT), DIMENSION(4) :: dst
      REAL(C_FLOAT), DIMENSION(3) :: va, vb
      dst(1) = DOT_PRODUCT(va, vb)
      CALL f_wedge_product((/dst(2), dst(3), dst(4)/), va, vb)
   END SUBROUTINE f_geometric_product

   SUBROUTINE f_rotate_vec_matrix(v, m) BIND (C)
      REAL(C_FLOAT), DIMENSION(3)   :: v
      REAL(C_FLOAT), DIMENSION(3,3) :: m
      REAL(KIND=4), DIMENSION(3)    :: w
      INTEGER :: i
      DO i = 1, 3
         w = w + m(:,i) * v(i)
      END DO
      v = w
   END SUBROUTINE f_rotate_vec_matrix

   SUBROUTINE f_construct_identity_matrix(m, n)
      REAL(KIND=4), DIMENSION(:, :) :: m
      INTEGER :: n, i
      DO i = 1, n
         m(i, i) = 1
      END DO
   END SUBROUTINE f_construct_identity_matrix

   FUNCTION f_construct_rot_matrix3(v)
      REAL(KIND=4), DIMENSION(3, 3) :: f_construct_rot_matrix3
      REAL(KIND=4), DIMENSION(3) :: v
      f_construct_rot_matrix3(1,:) = (/0.0, -1 * v(3), v(2)/)
      f_construct_rot_matrix3(2,:) = (/v(3), 0.0, -1 * v(1)/)
      f_construct_rot_matrix3(3,:) = (/-1 * v(2), v(1), 0.0/)
   END FUNCTION f_construct_rot_matrix3

END MODULE linalg
