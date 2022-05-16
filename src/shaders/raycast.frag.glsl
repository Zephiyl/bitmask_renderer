#version 460

struct cam_data_struct
{
  vec3  pos;
  float spacer_one;
  vec3  forward;
  float spacer_two;
  vec3  right;
  float spacer_three;
  vec3  up;
  float spacer_four;
  
  vec2 pix_offset;
  vec2 pix;
};

layout(binding = 0)
   readonly buffer cam_data_buffer
{
   cam_data_struct cam_data;
};

struct bitmap_type {
   uint bits[16];
};
struct bitchunk_type {
   bitmap_type  bitmap[4096];
   uint           indices[128];
};

layout(binding = 1)
   readonly buffer bit_data 
{
   bitchunk_type bit_world[64];
};

layout(location = 0) 
   in vec2 uv;
layout(location = 0) 
   out vec4 color;

const int   BITSIZE     = 8;
const int   CHUNKSIZE     = 16;
const int   WORLDSIZE     = 4;
const float EPSILON_SMALL = 0.01f;
const double COLCONST     = 1.0f/255.0f;

bool
intersect_bit(const vec3 origin, const vec3 direction, inout vec3 normal, inout float t, bitmap_type bitmap)
{
   ivec3 pos = ivec3(origin);

   ivec3 is_out;
   is_out[0] = direction[0] > 0.f ? BITSIZE : -1;
   is_out[1] = direction[1] > 0.f ? BITSIZE : -1;
   is_out[2] = direction[2] > 0.f ? BITSIZE : -1;

   vec3 dir_step;
   dir_step[0] = direction[0] > 0.f ? 1.f : -1.f;
   dir_step[1] = direction[1] > 0.f ? 1.f : -1.f;
   dir_step[2] = direction[2] > 0.f ? 1.f : -1.f;
   vec3 dir_inv = vec3(1.f / direction[0], 1.f / direction[1], 1.f / direction[2]);
   vec3 delta   = vec3(dir_step[0] * dir_inv[0], dir_step[1] * dir_inv[1], dir_step[2] * dir_inv[2]);

   vec3 bounds;
   bounds[0] = direction[0] > 0.f ? pos[0] + 1 : pos[0];
   bounds[1] = direction[1] > 0.f ? pos[1] + 1 : pos[1];
   bounds[2] = direction[2] > 0.f ? pos[2] + 1 : pos[2];
   vec3 max;
   max[0] = direction[0] != 0.f ? (bounds[0] - origin[0]) * dir_inv[0] : 10000.f;
   max[1] = direction[1] != 0.f ? (bounds[1] - origin[1]) * dir_inv[1] : 10000.f;
   max[2] = direction[2] != 0.f ? (bounds[2] - origin[2]) * dir_inv[2] : 10000.f;

   pos[0] = pos[0] % BITSIZE;
   pos[1] = pos[1] % BITSIZE;
   pos[2] = pos[2] % BITSIZE;

   int delta_axis = -1;
   vec3 delta_mask = vec3(0, 0, 0); //f32 instead of bool for math later
   while (true) {
      int index     = pos[0] + (pos[1] * BITSIZE) + (pos[2] * BITSIZE * BITSIZE);
      int array_pos = index / 32;
      int bit_pos   = index % 32;

      if (bitfieldExtract(bitmap.bits[array_pos], bit_pos, 1) != 0) 
      {
         t = 0;
         if (delta_axis > -1) {
            normal[0] = 0;
            normal[1] = 0;
            normal[2] = 0;
            normal[delta_axis] = -1 * dir_step[delta_axis];
            t = max[delta_axis] - delta[delta_axis];
         }
         return true;
      }
      delta_axis    = (max[0] < max[1]) ? ((max[0] < max[2]) ? 0 : 2) : ((max[1] < max[2]) ? 1 : 2);
      delta_mask[0] = ((max[0] <  max[1]) && (max[0] <  max[2])) ? 1.f : 0.f;
      delta_mask[1] = ((max[1] <= max[0]) && (max[1] <  max[2])) ? 1.f : 0.f;
      delta_mask[2] = ((max[2] <= max[0]) && (max[2] <= max[1])) ? 1.f : 0.f;

      pos = ivec3(pos + (delta_mask * dir_step)); 
      if (pos[delta_axis] == is_out[delta_axis]) 
      {
         return false;
      }
      max[0] += (delta_mask[0] * delta[0]);
      max[1] += (delta_mask[1] * delta[1]);
      max[2] += (delta_mask[2] * delta[2]);
   }
}

bool
intersect_aabb(const vec3 direction, const vec3 pos, out float t)
{
   vec3 box_min = vec3(0.f, 0.f, 0.f);
   vec3 box_max = vec3(CHUNKSIZE * WORLDSIZE, CHUNKSIZE * WORLDSIZE, CHUNKSIZE * WORLDSIZE);

   vec3 t0 = vec3((box_min - pos) / direction);
   vec3 t1 = vec3((box_max - pos) / direction);

   vec3 t_min = min(t0, t1);
   vec3 t_max = max(t0, t1);

   t         = max(max(t_min[0], 0.f), max(t_min[1], t_min[2]));
   float t_m = min(t_max[0], min(t_max[1], t_max[2]));
   return t_m > t;
}

bool
digital_differential_analyzer(vec3 direction, inout vec3 normal, inout float t)
{
   t = 0;
   if (!intersect_aabb(direction, cam_data.pos, t)) 
   {
      return false;
   }
   vec3 origin; 
   origin[0] = cam_data.pos[0] + (direction[0] * t);
   origin[1] = cam_data.pos[1] + (direction[1] * t); 
   origin[2] = cam_data.pos[2] + (direction[2] * t);

   const vec3 scale  = vec3(1.f / CHUNKSIZE, 1.f / CHUNKSIZE, 1.f / CHUNKSIZE);
   const vec3 center = vec3(CHUNKSIZE / 2.f, CHUNKSIZE / 2.f, CHUNKSIZE / 2.f);

   vec3 dist_to_center;
   dist_to_center[0] = abs(center[0] - origin[0]) * scale[0];
   dist_to_center[1] = abs(center[1] - origin[1]) * scale[1];
   dist_to_center[2] = abs(center[2] - origin[2]) * scale[2];
   vec3 signed;
   signed[0] = sign(origin[0] - center[0]);
   signed[1] = sign(origin[1] - center[1]);
   signed[2] = sign(origin[2] - center[2]);
   float center_max   = max(dist_to_center[0], max(dist_to_center[1], dist_to_center[2]));
   dist_to_center[0] /= center_max;
   dist_to_center[1] /= center_max;
   dist_to_center[2] /= center_max;

   normal[0] = signed[0] * trunc(dist_to_center[0] + 0.000001f);
   normal[1] = signed[1] * trunc(dist_to_center[1] + 0.000001f);
   normal[2] = signed[2] * trunc(dist_to_center[2] + 0.000001f);
   
   origin[0] -= (normal[0] * EPSILON_SMALL);
   origin[1] -= (normal[1] * EPSILON_SMALL);
   origin[2] -= (normal[2] * EPSILON_SMALL);

   ivec3 pos = ivec3(origin);

   ivec3 is_out;
   is_out[0] = direction[0] > 0.f ? CHUNKSIZE * WORLDSIZE: -1;
   is_out[1] = direction[1] > 0.f ? CHUNKSIZE * WORLDSIZE: -1;
   is_out[2] = direction[2] > 0.f ? CHUNKSIZE * WORLDSIZE: -1;

   vec3 dir_step;
   dir_step[0]   = direction[0] > 0.f ? 1.f : -1.f;
   dir_step[1]   = direction[1] > 0.f ? 1.f : -1.f;
   dir_step[2]   = direction[2] > 0.f ? 1.f : -1.f;
   vec3 dir_inv  = vec3(1.f / direction[0], 1.f / direction[1], 1.f / direction[2]);
   vec3 delta    = vec3(dir_step[0] * dir_inv[0], dir_step[1] * dir_inv[1], dir_step[2] * dir_inv[2]);

   vec3 bounds;
   bounds[0] = direction[0] > 0.f ? pos[0] + 1 : pos[0];
   bounds[1] = direction[1] > 0.f ? pos[1] + 1 : pos[1];
   bounds[2] = direction[2] > 0.f ? pos[2] + 1 : pos[2];
   vec3 max;
   max[0] = direction[0] != 0.f ? (bounds[0] - origin[0]) * dir_inv[0] : 1000000.f;
   max[1] = direction[1] != 0.f ? (bounds[1] - origin[1]) * dir_inv[1] : 1000000.f;
   max[2] = direction[2] != 0.f ? (bounds[2] - origin[2]) * dir_inv[2] : 1000000.f;

   int delta_axis  = -1;
   vec3 delta_mask = vec3(0, 0, 0); //f32 instead of bool for math later
   float t_local   = 0;
   float t_bit   = 0;
   while (true) {
      int index_chunk = ((pos[0] / CHUNKSIZE) % WORLDSIZE)+(((pos[1] / CHUNKSIZE) % WORLDSIZE) * WORLDSIZE)+(((pos[2] / CHUNKSIZE) % WORLDSIZE)*WORLDSIZE * WORLDSIZE);
      int index_bitmap = (pos[0] % CHUNKSIZE) + ((pos[1] % CHUNKSIZE) * CHUNKSIZE) + ((pos[2] % CHUNKSIZE) * CHUNKSIZE * CHUNKSIZE);
      int index_array = index_bitmap / 32;
      int index_bit   = index_bitmap % 32;
   
      if (bitfieldExtract(bit_world[index_chunk].indices[index_array], index_bit, 1) != 0)
      {
         t_local = 0;
         if (delta_axis > -1) {
            normal[0] = 0; 
            normal[1] = 0; 
            normal[2] = 0; 
            normal[delta_axis] = -1.f * dir_step[delta_axis];
            t_local = (max[delta_axis] - delta[delta_axis]);
         }
         vec3 local_origin;
         local_origin[0] = ((origin[0] + (direction[0] * t_local)) * 8.f) - (normal[0] * EPSILON_SMALL);
         local_origin[1] = ((origin[1] + (direction[1] * t_local)) * 8.f) - (normal[1] * EPSILON_SMALL);
         local_origin[2] = ((origin[2] + (direction[2] * t_local)) * 8.f) - (normal[2] * EPSILON_SMALL);
         if (intersect_bit(local_origin, direction, normal, t_bit, bit_world[index_chunk].bitmap[index_bitmap])) 
         {
            t = t + t_local + (t_bit * 0.125);
            return true;
         }
      }
      delta_axis    = (max[0] < max[1]) ? ((max[0] < max[2]) ? 0 : 2) : ((max[1] < max[2]) ? 1 : 2);
      delta_mask[0] = ((max[0] <  max[1]) && (max[0] <  max[2])) ? 1.f : 0.f;
      delta_mask[1] = ((max[1] <= max[0]) && (max[1] <  max[2])) ? 1.f : 0.f;
      delta_mask[2] = ((max[2] <= max[0]) && (max[2] <= max[1])) ? 1.f : 0.f;

      pos = ivec3(pos + (delta_mask * dir_step));
      if (pos[delta_axis] == is_out[delta_axis]) 
      {
         return false;
      }
      max[0] += (delta_mask[0] * delta[0]);
      max[1] += (delta_mask[1] * delta[1]);
      max[2] += (delta_mask[2] * delta[2]);
   }
}

uint
normal_shade(vec3 normal)
{
   if (normal[1] > 0) {
      return 16;
   } else if (normal[0] > 0) {
      return 12;
   } else if (normal[0] < 0) {
      return 8;
   } else if (normal[1] < 0) {
      return 4;
   } else {
      return 0;
   }
}

void
main(void)
{
   vec4 colour_out = vec4(0, 0, 0, 0);
   vec2 dir_mask = vec2(cam_data.pix + (cam_data.pix_offset * uv));

   vec3 direction;
   direction[0] = cam_data.forward[0] + (dir_mask[0] * cam_data.right[0]) + (dir_mask[1] * cam_data.up[0]);
   direction[1] = cam_data.forward[1] + (dir_mask[0] * cam_data.right[1]) + (dir_mask[1] * cam_data.up[1]);
   direction[2] = cam_data.forward[2] + (dir_mask[0] * cam_data.right[2]) + (dir_mask[1] * cam_data.up[2]);
   normalize(direction);
   
   vec3 normal = vec3(0, 0, 0);
   float t     = 0;
   if(digital_differential_analyzer(direction, normal, t))
   {
      uint shade = normal_shade(normal);
      colour_out = vec4((19+shade)*COLCONST, (143+shade)*COLCONST, (205+shade)*COLCONST, 1.f);
   } else 
   {
      colour_out = vec4(uv, 0.5f, 1.f); 
   }
   color = colour_out;
}
