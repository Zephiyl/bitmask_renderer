#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <malloc.h>
#include <math.h>
#include <sys/time.h>

#include "ext/volk.h"
#include <GLFW/glfw3.h>

#include "util/util_typedefs.h"

#include "gpu/vulk_interface.h"

#include "init.h"

#include "voxel/bitmap.h"
#include "math/fmath.h"
#include "procgen/noise.h"

typedef struct 
{
   f32 pos[3];
   f32 spacer_one;
   f32 forward[3];
   f32 spacer_two;
   f32 right[3];
   f32 spacer_three;
   f32 up[3];
   f32 spacer_four;

   f32 pix_offset[2];
   f32 pix[2];
} cam_data_struct;

void
cam_move(GLFWwindow* g_window, cam_data_struct* cam_data, f32* angle, bool* mouse_toggle)
{  
   s32 window[2] = {0};
   f64 cursor[2] = {0};
   
   if (*mouse_toggle) {
      glfwGetCursorPos(g_window, &cursor[0], &cursor[1]);
      glfwGetWindowSize(g_window, &window[0], &window[1]);
      glfwSetCursorPos(g_window, window[0] * 0.5, window[1] * 0.5);

      angle[0] -= ((cursor[0] - (window[0] * 0.5)) * 0.024); // yaw
      angle[1] += ((cursor[1] - (window[1] * 0.5)) * 0.024); // pitch

      f32 bi_xz[3]    = {0};
      f32 bi_yz[3]    = {0};
      f32 rotor_yaw[4]   = {0};
      f32 rotor_pitch[4] = {0};
      f_wedge_product(bi_xz, cam_data->right,   cam_data->forward);
      f_construct_rotor_deg(rotor_yaw,   bi_xz, &angle[0]);
      f_normalize_rotor_fast(rotor_yaw);
      f_rotate_vector(cam_data->forward,  rotor_yaw);
      f_normalize_v3_fast(cam_data->forward);

      f_wedge_product(bi_yz, cam_data->up, cam_data->forward);
      f_construct_rotor_deg(rotor_pitch,  bi_yz, &angle[1]);
      f_normalize_rotor_fast(rotor_pitch);
      f_rotate_vector(cam_data->forward,  rotor_pitch);
      
      f_cross_product(cam_data->right, cam_data->forward, (f32[3]){0.f, 1.f, 0.f});
      f_cross_product(cam_data->up, cam_data->right, cam_data->forward);
   }
   f_normalize_v3_fast(cam_data->up);
   f_normalize_v3_fast(cam_data->forward);
   f_normalize_v3_fast(cam_data->right);
   angle[0] = 0;
   angle[1] = 0; 

   glfwPollEvents();
   if (glfwGetKey(g_window, GLFW_KEY_W)) 
   {
      cam_data->pos[0] += (cam_data->forward[0] * 0.15);
      cam_data->pos[1] += (cam_data->forward[1] * 0.15);
      cam_data->pos[2] += (cam_data->forward[2] * 0.15);
   }
   else if (glfwGetKey(g_window, GLFW_KEY_S)) 
   {
      cam_data->pos[0] -= (cam_data->forward[0] * 0.15);
      cam_data->pos[1] -= (cam_data->forward[1] * 0.15);
      cam_data->pos[2] -= (cam_data->forward[2] * 0.15);
   }

   if (glfwGetKey(g_window, GLFW_KEY_D)) 
   {
      cam_data->pos[0] += (cam_data->right[0] * 0.15);
      cam_data->pos[1] += (cam_data->right[1] * 0.15);
      cam_data->pos[2] += (cam_data->right[2] * 0.15);
   }
   else if (glfwGetKey(g_window, GLFW_KEY_A)) 
   {
      cam_data->pos[0] -= (cam_data->right[0] * 0.15);
      cam_data->pos[1] -= (cam_data->right[1] * 0.15);
      cam_data->pos[2] -= (cam_data->right[2] * 0.15);
   }

   if (glfwGetKey(g_window, GLFW_KEY_SPACE)) 
   {
      cam_data->pos[0] += (cam_data->up[0] * 0.15);
      cam_data->pos[1] += (cam_data->up[1] * 0.15);
      cam_data->pos[2] += (cam_data->up[2] * 0.15);
   }
   else if (glfwGetKey(g_window, GLFW_KEY_LEFT_CONTROL)) 
   {
      cam_data->pos[0] -= (cam_data->up[0] * 0.15);
      cam_data->pos[1] -= (cam_data->up[1] * 0.15);
      cam_data->pos[2] -= (cam_data->up[2] * 0.15);
   }
   
   if (glfwGetKey(g_window, GLFW_KEY_V) && !*mouse_toggle)
   {
      glfwSetInputMode(g_window, GLFW_CURSOR, GLFW_CURSOR_HIDDEN);
      *mouse_toggle = true;
   }
   else if (glfwGetKey(g_window, GLFW_KEY_V) && *mouse_toggle)
   {
      glfwSetInputMode(g_window, GLFW_CURSOR, GLFW_CURSOR_NORMAL);
      *mouse_toggle = false;
   }
}

struct timeval timev;

u64
utime(void)
{
   gettimeofday(&timev, 0);
   return (timev.tv_sec * 1000000LU + timev.tv_usec) / 1000;
}

int 
main(void) 
{ 
   const char* application_name = "Bitmask Renderer";

   //INIT BACKEND
   vulk_data_struct   vulk_data = {0};
   image_struct       targets[2];
   render_data_struct render_data = {0};
   
   render_data.render_memory_barrier_count = 2;
   render_data.clear_values_count = 2;
   render_data.image_copy_memory_barrier_count = 2;
   
   init_glfw   (&vulk_data, application_name);
   init_vulkan (&vulk_data, application_name);
   init_surface(&vulk_data);

   init_shaders        (&vulk_data);
   init_render_pass    (&vulk_data);
   init_descriptor_set (&vulk_data);
   init_pipeline       (&vulk_data);

   init_swapchain (&vulk_data);

   init_image_struct
      (&vulk_data, &targets[0], 
       vulk_data.colour_format, 
       VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT | VK_IMAGE_USAGE_TRANSFER_SRC_BIT,
       VK_IMAGE_ASPECT_COLOR_BIT);
   init_image_struct
      (&vulk_data, &targets[1], 
       vulk_data.depth_format, 
       VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT,
       VK_IMAGE_ASPECT_DEPTH_BIT);
   
   init_render_data(&vulk_data, &render_data, targets);
   init_misc_indices(&vulk_data);

   //INIT CAM DATA
   cam_data_struct cam_data = {0};
   {
      const f32 base = 0.5f;
      const f32 fov = 1.f / tanf((65.f / (180/3.1415f))/2.0f);
      const f32 scale[2] = {base * fov, base};

      cam_data.pos[0]      = 32;    
      cam_data.pos[1]      = 48;    
      cam_data.pos[2]      = -32;    
      cam_data.forward[2]  = 1;    
      cam_data.right[0]    = 1;    
      cam_data.up[1]       = 1;

      cam_data.pix_offset[0] = scale[0];
      cam_data.pix_offset[1] = scale[1];
      cam_data.pix[0]        = -1 * (scale[0] / 2);      
      cam_data.pix[1]        = -1 * (scale[1] / 2);      

   };
   //INIT BUFFERS
   size_t bit_world_alloc = (128 + 4096*16) * 4 * 64;

#define WORLD_BUFFER_FLAGS (VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT)
   struct vulk_buffer scratch      = {0};
   struct vulk_buffer cam          = {0};
   struct vulk_buffer bit_world_buffer = {0};

   create_buffer(&vulk_data, &scratch, 24 * 1024 * 1024, VK_BUFFER_USAGE_TRANSFER_SRC_BIT, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);
   create_buffer(&vulk_data, &cam,     512, VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT);
   create_buffer(&vulk_data, &bit_world_buffer, bit_world_alloc, WORLD_BUFFER_FLAGS, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT);
   
   bitchunk_type* bit_world = malloc(bit_world_alloc);
  
   s32 world_seed = 1004569;
   generate_world(bit_world, world_seed);

   fill_buffer(&vulk_data, scratch, &cam, &cam_data, sizeof(cam_data));
   fill_buffer(&vulk_data, scratch, &bit_world_buffer, bit_world, bit_world_alloc);

   float angle[3] = {0, 0, 0.1};
   bool mouse_toggle = false;
   u64 timer[2] = {0, 0};

   while (!glfwWindowShouldClose(vulk_data.window)) 
   {
      timer[0] = utime();
      cam_move(vulk_data.window, &cam_data, angle, &mouse_toggle);

      fill_buffer(&vulk_data, scratch, &cam, &cam_data, sizeof(cam_data));
      if (resize_swapchain(&vulk_data))
      {
         resize_image_struct(&vulk_data, &targets[0]);
         resize_image_struct(&vulk_data, &targets[1]);

         render_data.target_framebuffer = create_framebuffer(&vulk_data, targets, 2);
      }
       
      vkAcquireNextImageKHR(vulk_data.logical_device, vulk_data.swapchain, ~0ull, render_data.semaphore[0], VK_NULL_HANDLE, &render_data.image_index);
      
      vkDeviceWaitIdle(vulk_data.logical_device);
      vkResetCommandPool(vulk_data.logical_device, vulk_data.command_pool[render_data.graph_index], 0);
      vkBeginCommandBuffer(vulk_data.command_buffer[render_data.graph_index], &render_data.command_buffer_info);

      render_data.render_memory_barrier[0] = create_image_barrier
          (targets[0].image, 0, 0,
          VK_IMAGE_LAYOUT_UNDEFINED,
          VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, 
          VK_IMAGE_ASPECT_COLOR_BIT);
      render_data.render_memory_barrier[1] = create_image_barrier
          (targets[1].image, 0, 0,
          VK_IMAGE_LAYOUT_UNDEFINED,
          VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL, 
          VK_IMAGE_ASPECT_DEPTH_BIT);

      vkCmdPipelineBarrier
         (vulk_data.command_buffer[render_data.graph_index], 
          VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT, 
          VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT | VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT, 
          VK_DEPENDENCY_BY_REGION_BIT, 
          0, 0, 0, 0, 
          render_data.render_memory_barrier_count,
          render_data.render_memory_barrier);
      
      render_data.render_pass_info.framebuffer = render_data.target_framebuffer;
      render_data.render_pass_info.renderArea.extent.width  = vulk_data.window_dim[0];
      render_data.render_pass_info.renderArea.extent.height = vulk_data.window_dim[1];
      
      vkCmdBeginRenderPass(vulk_data.command_buffer[render_data.graph_index], &render_data.render_pass_info, VK_SUBPASS_CONTENTS_INLINE);

      render_data.viewport.y            = (f32)vulk_data.window_dim[1];
      render_data.viewport.width        = (f32)vulk_data.window_dim[0];
      render_data.viewport.height       = -1.0f * (f32)vulk_data.window_dim[1];
      render_data.scissor.extent.width  = vulk_data.window_dim[0];
      render_data.scissor.extent.height = vulk_data.window_dim[1];

      vkCmdSetViewport(vulk_data.command_buffer[render_data.graph_index], 0, 1, &render_data.viewport);
      vkCmdSetScissor(vulk_data.command_buffer[render_data.graph_index], 0, 1, &render_data.scissor);
      vkCmdBindPipeline(vulk_data.command_buffer[render_data.graph_index], VK_PIPELINE_BIND_POINT_GRAPHICS, vulk_data.pipeline);

      VkDescriptorBufferInfo world_buffer_info = {0};

      world_buffer_info.buffer = bit_world_buffer.buffer;
      world_buffer_info.offset = 0;
      world_buffer_info.range  = bit_world_buffer.size;

      VkDescriptorBufferInfo cam_buffer_info = {0};
      cam_buffer_info.buffer = cam.buffer;
      cam_buffer_info.offset = 0;
      cam_buffer_info.range  = cam.size;

      VkWriteDescriptorSet write_desc_set[2] = {0};
      write_desc_set[0].sType           = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
      write_desc_set[0].pNext           = NULL;
      write_desc_set[0].dstBinding      = 0;
      write_desc_set[0].descriptorCount = 1;
      write_desc_set[0].descriptorType  = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
      write_desc_set[0].pBufferInfo     = &cam_buffer_info;

      write_desc_set[1].sType           = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
      write_desc_set[1].pNext           = NULL;
      write_desc_set[1].dstBinding      = 1;
      write_desc_set[1].descriptorCount = 1;
      write_desc_set[1].descriptorType  = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
      write_desc_set[1].pBufferInfo     = &world_buffer_info;

      vkCmdPushDescriptorSetKHR
         (vulk_data.command_buffer[render_data.graph_index],
          VK_PIPELINE_BIND_POINT_GRAPHICS,
          vulk_data.pipeline_layout, 0,
          2, write_desc_set);

      vkCmdDraw(vulk_data.command_buffer[render_data.graph_index], 3, 1, 0, 0);
      vkCmdEndRenderPass(vulk_data.command_buffer[render_data.graph_index]);
      
      render_data.image_copy_memory_barrier[0] = create_image_barrier
          (targets[0].image, 
          VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT, 
          VK_ACCESS_TRANSFER_READ_BIT,
          VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
          VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL, 
          VK_IMAGE_ASPECT_COLOR_BIT);
      render_data.image_copy_memory_barrier[1] = create_image_barrier
          (vulk_data.images[render_data.image_index], 0, 
          VK_ACCESS_TRANSFER_WRITE_BIT,
          VK_IMAGE_LAYOUT_UNDEFINED,
          VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, 
          VK_IMAGE_ASPECT_COLOR_BIT);
      
      vkCmdPipelineBarrier
         (vulk_data.command_buffer[render_data.graph_index], 
          VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT, 
          VK_PIPELINE_STAGE_TRANSFER_BIT,
          VK_DEPENDENCY_BY_REGION_BIT,
          0, 0, 0, 0,
          render_data.image_copy_memory_barrier_count,
          render_data.image_copy_memory_barrier);

      render_data.image_copy.extent.width  = vulk_data.window_dim[0];
      render_data.image_copy.extent.height = vulk_data.window_dim[1];

      vkCmdCopyImage
         (vulk_data.command_buffer[render_data.graph_index], 
          targets[0].image, 
          VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
          vulk_data.images[render_data.image_index], 
          VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, 
          1, &render_data.image_copy);

      render_data.present_memory_barrier = create_image_barrier
          (vulk_data.images[render_data.image_index], 
          VK_ACCESS_TRANSFER_WRITE_BIT, 0,
          VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
          VK_IMAGE_LAYOUT_PRESENT_SRC_KHR, 
          VK_IMAGE_ASPECT_COLOR_BIT);

      vkCmdPipelineBarrier
         (vulk_data.command_buffer[render_data.graph_index],
          VK_PIPELINE_STAGE_TRANSFER_BIT,
          VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT,
          VK_DEPENDENCY_BY_REGION_BIT,
          0, 0, 0, 0, 
          1, &render_data.present_memory_barrier);

      vkEndCommandBuffer(vulk_data.command_buffer[render_data.graph_index]);

      vkQueueSubmit(vulk_data.queue[render_data.graph_index], 1, &render_data.submit_info, VK_NULL_HANDLE);
      vkQueuePresentKHR(vulk_data.queue[render_data.graph_index], &render_data.present_info);
      timer[1] = utime() - timer[0];
      //Uncomment for frametime printing.
      //printf("frametime: %lu ms(%f fps)\n", timer[1], 1.0 / (timer[1] / 1000.0));
   }

   glfwDestroyWindow(vulk_data.window);
   glfwTerminate();
   return 0;
}
