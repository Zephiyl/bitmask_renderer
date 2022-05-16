#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <math.h>

#include "../util/util_typedefs.h"
#include "../util/util_constants.h"
#include "../util/util_macros.h"

#include "bitmap.h"
#include "../procgen/noise.h"

u32 WORLDSIZE = 4;
u32 CHUNKSIZE = 16;
u32 BITSIZE = 8;

void
fill_chunk(bitchunk_type* bitchunk) 
{
   u32 filled_bytes = 0xFFFFFFFF;
   for (u16 i = 0; i < UINT16_MAX; i++)
   {
      *((u32 *)bitchunk + i) = filled_bytes;
   }
   for (u16 i = 0; i < 128; i++)
   {
      bitchunk->indices[i] = filled_bytes;
   }
}

void
generate_chunk(bitchunk_type* bitchunk, u32* chk_pos, s32 seed)
{  
   s32 vox_pos[3] = {0, 0, 0};
   s32 height   = 0;
   f64 height_f = 0.0;
   for (u32 i = 0; i < 16384; i++)
   {
      vox_pos[0] = i % 128;
      vox_pos[2] = floor(i / 128);
      height_f = perlin_noise((chk_pos[0] * 128) + vox_pos[0], (chk_pos[2] * 128) + vox_pos[2], 0.01, 2, seed);
      height = floor((((WORLDSIZE -1) * 128 * height_f) - ((chk_pos[1] -1) * 128)) + EPSILON);

      //Fix height being over 127
      for (vox_pos[1] = 0; vox_pos[1] < height; vox_pos[1]++)
      {
         u16 bit_index =(vox_pos[0] / BITSIZE) + ((vox_pos[1] % 128) / BITSIZE) * CHUNKSIZE + (vox_pos[2]/BITSIZE) * CHUNKSIZE * CHUNKSIZE; 
         u16 vox_index =(vox_pos[0] % BITSIZE) + (vox_pos[1] % BITSIZE) * BITSIZE + (vox_pos[2]%BITSIZE) * BITSIZE * BITSIZE; 
         u16 bit_index_arr = vox_index / 32; 
         u16 bit_shift = vox_index % 32;
         bitchunk->bitmap[bit_index].bits[bit_index_arr]  |= (1 << bit_shift);
         bitchunk->indices[bit_index / 32] |= (1 << bit_shift);
      }
   }
}

void
generate_world(bitchunk_type* bit_world, s32 seed)
{
   u32 index_chunk = 0;
   for (u32 i = 0; i < WORLDSIZE; i++)
   {
      for (u32 j = 0; j < WORLDSIZE; j++)
      {
         index_chunk = i + (j * WORLDSIZE * WORLDSIZE);
         fill_chunk(&bit_world[index_chunk]);
      }
   }
   
   for (u32 i = 0; i < WORLDSIZE; i++)
   {
      for (u32 j = 1; j < WORLDSIZE; j++)
      {
         for (u32 k = 0; k < WORLDSIZE; k++)
         {
            u32 chk_pos[3] = {i, j, k};
            index_chunk = i + (j * WORLDSIZE) + (k * WORLDSIZE * WORLDSIZE);

            generate_chunk(&bit_world[index_chunk], chk_pos, seed);
         }
      }
   }
}

