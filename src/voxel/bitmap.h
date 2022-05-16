/*Uses:
 * util/util_typedefs.h
 */

extern u32 WORLDSIZE;
extern u32 CHUNKSIZE;
extern u32 BITSIZE;

typedef struct {
   u32 bits[16];
} bitmap_type;

typedef struct bitchunk_type {
   bitmap_type  bitmap[4096];
   u32            indices[128];
} bitchunk_type;

void
generate_world(bitchunk_type* bit_world, s32 seed);
