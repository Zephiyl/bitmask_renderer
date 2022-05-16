
MODULE fast_math
   USE, INTRINSIC :: iso_c_binding
   IMPLICIT NONE

CONTAINS

   SUBROUTINE f_normalize_v3_fast(v) BIND (C)
      REAL(C_FLOAT), DIMENSION(3) :: v 
      REAL(KIND=4) :: mag
      mag = 1.0 / SQRT((v(1)**2) + (v(2)**2) + (v(3)**2))
      v(1) = v(1) * mag
      v(2) = v(2) * mag
      v(3) = v(3) * mag
   END SUBROUTINE f_normalize_v3_FAST

   SUBROUTINE f_normalize_rotor_fast(rot) BIND (C)
      REAL(C_FLOAT), DIMENSION(4) :: rot
      REAL(KIND=4) :: mag
      mag = 1.0 / SQRT((rot(1)**2) + (rot(2)**2) + (rot(3)**2) + (rot(4)**2))
      rot(1) = rot(1) * mag
      rot(2) = rot(2) * mag
      rot(3) = rot(3) * mag
      rot(4) = rot(4) * mag
   END SUBROUTINE f_normalize_rotor_FAST

END MODULE fast_math
