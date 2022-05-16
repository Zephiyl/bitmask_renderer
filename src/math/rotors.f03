
MODULE rotors
   USE, INTRINSIC :: iso_c_binding
   USE :: linalg
   IMPLICIT NONE

CONTAINS

   SUBROUTINE f_construct_rotor_rad(dst, bi, rad) BIND (C)
      REAL(C_FLOAT), DIMENSION(4) :: dst
      REAL(C_FLOAT), DIMENSION(3) :: bi
      REAL(C_FLOAT) :: rad
      REAL(KIND=4)  :: sin_rad
      dst(1) = COS(rad / 2)
      sin_rad = -1 * SIN(rad / 2)
      dst(2) = sin_rad * bi(1) 
      dst(3) = sin_rad * bi(2) 
      dst(4) = sin_rad * bi(3)
   END SUBROUTINE f_construct_rotor_rad

   SUBROUTINE f_construct_rotor_deg(dst, bi, deg) BIND (C)
      REAL(C_FLOAT), DIMENSION(4) :: dst
      REAL(C_FLOAT), DIMENSION(3) :: bi
      REAL(C_FLOAT) :: deg
      REAL(KIND=4)  :: rad
      rad = deg / (180 / 3.1415)
      CALL f_construct_rotor_rad(dst, bi, rad)
   END SUBROUTINE f_construct_rotor_deg

   SUBROUTINE f_construct_rotor_from_to(dst, from, to) BIND (C)
      REAL(C_FLOAT), DIMENSION(4) :: dst
      REAL(C_FLOAT), DIMENSION(3) :: from, to
      dst(1) = DOT_PRODUCT(to, from)
      CALL f_wedge_product((/dst(2), dst(3), dst(4)/), to, from)
   END SUBROUTINE f_construct_rotor_from_to

   SUBROUTINE f_mult_rotor(dst, ra, rb) BIND (C)
      REAL(C_FLOAT), DIMENSION(4) :: dst, ra, rb
      dst(1) = (ra(1) * rb(1)) - (ra(2) * rb(2)) - (ra(3) * rb(3)) - (ra(4) * rb(4))
      dst(2) = (ra(2) * rb(1)) + (ra(1) * rb(2)) + (ra(4) * rb(3)) - (ra(3) * rb(4))
      dst(3) = (ra(3) * rb(1)) + (ra(1) * rb(3)) - (ra(4) * rb(2)) + (ra(2) * rb(4))
      dst(4) = (ra(4) * rb(1)) + (ra(1) * rb(4)) + (ra(3) * rb(2)) - (ra(2) * rb(3))
   END SUBROUTINE f_mult_rotor

   SUBROUTINE f_rotate_vector(v, rotor) BIND (C) 
      REAL(C_FLOAT), DIMENSION(3) :: v
      REAL(C_FLOAT), DIMENSION(4) :: rotor
      REAL(KIND=4), DIMENSION(3)  :: vb
      vb(1) = (rotor(1) * v(1)) + (v(2) * rotor(2)) + (v(3) * rotor(3))
      vb(2) = (rotor(1) * v(2)) - (v(1) * rotor(2)) + (v(3) * rotor(4))
      vb(3) = (rotor(1) * v(3)) - (v(1) * rotor(3)) - (v(2) * rotor(4))

      v(1) = (rotor(1) * vb(1)) + (vb(2) * rotor(2)) + (vb(3) * rotor(3))
      v(2) = (rotor(1) * vb(2)) - (vb(1) * rotor(2)) + (vb(3) * rotor(4))
      v(3) = (rotor(1) * vb(3)) - (vb(1) * rotor(3)) - (vb(2) * rotor(4))
   END SUBROUTINE f_rotate_vector

   SUBROUTINE f_rotate_rotor(dst, rot) BIND (C)
      REAL(C_FLOAT), DIMENSION(4) :: dst, rot
      REAL(KIND=4), DIMENSION(4) :: scratch, rev
      
      rev(1) = dst(1)
      rev(2) = dst(2) * (-1)
      rev(3) = dst(3) * (-1)
      rev(4) = dst(4) * (-1)
      CALL f_mult_rotor(scratch, dst,     rot)
      CALL f_mult_rotor(dst,     scratch, rev) 
   END SUBROUTINE f_rotate_rotor

END MODULE rotors
