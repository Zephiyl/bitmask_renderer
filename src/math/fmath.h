/*Uses:
 * util/util_typedefs.h
 */
   //fast_math.f03
extern void f_normalize_v3_fast    (f32* v);
extern void f_normalize_rotor_fast (f32* rot);
   //rotors.f03
extern void f_construct_rotor_rad     (f32* dst, f32* bi,   f32* rad);
extern void f_construct_rotor_deg     (f32* dst, f32* bi,   f32* deg);
extern void f_construct_rotor_from_to (f32* dst, f32* from, f32* to);

extern void f_mult_rotor         (f32* dst, f32* ra, f32* rb);
extern void f_rotate_vector      (f32* v,   f32* rotor);
extern void f_rotate_rotor       (f32* dst, f32* rot);
   //linalg.f03
extern void f_cross_product      (f32* dst, f32* va, f32* vb);
extern void f_dot_product        (f32* dst, f32* va, f32* vb);
extern void f_wedge_product      (f32* dst, f32* va, f32* vb);
extern void f_geometric_product  (f32* dst, f32* va, f32* vb);

extern void f_rotate_vec_matrix  (f32* v, f32** m);
