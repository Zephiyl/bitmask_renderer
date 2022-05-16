#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "../util/util_typedefs.h"

#include "../ext/volk.h"
#include <GLFW/glfw3.h>

#include "vulk_interface.h"

void
load_shader(VULK_DATA_INC, VkShaderModule* shader, const char* shader_path)
{ 
   FILE* shader_file;
   size_t file_length;
   VkShaderModuleCreateInfo shader_info = {0};

   shader_file = fopen(shader_path, "rb");
   fseek(shader_file, 0, SEEK_END);
   file_length = ftell(shader_file);
   fseek(shader_file, 0, SEEK_SET);

   u8 shader_buffer[file_length];
   fread(shader_buffer, 1, file_length, shader_file);
   fclose(shader_file);

   shader_info.sType    = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
   shader_info.pNext    = NULL;
   shader_info.codeSize = file_length;
   shader_info.pCode    = (u32*)shader_buffer;

   vkCreateShaderModule(vulk_data->logical_device, &shader_info, NULL, shader);
}

void
get_window_size(VULK_DATA_INC)
{
   glfwGetWindowSize(vulk_data->window, &vulk_data->window_dim[0], &vulk_data->window_dim[1]);
}

VkSemaphore
create_semaphore(VULK_DATA_INC)
{
   VkSemaphore return_semaphore;
   VkSemaphoreCreateInfo semaphore_info = {0};
   semaphore_info.sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;
   semaphore_info.pNext = NULL;

   vkCreateSemaphore(vulk_data->logical_device, &semaphore_info, NULL, &return_semaphore);
   return return_semaphore;
}

bool
resize_swapchain(VULK_DATA_INC)
{
   s32                      window_copy[2];
   VkSwapchainCreateInfoKHR swapchain_info = {0};

   glfwGetWindowSize(vulk_data->window, &window_copy[0], &window_copy[1]);
   if ((vulk_data->window_dim[0] == window_copy[0]) && (vulk_data->window_dim[1] == window_copy[1]))
   {
      return false;
   }
   vulk_data->window_dim[0] = window_copy[0];
   vulk_data->window_dim[1] = window_copy[1];

   swapchain_info.sType                 = VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR;
   swapchain_info.pNext                 = NULL;
   swapchain_info.surface               = vulk_data->surface;
   swapchain_info.minImageCount         = vulk_data->surface_capabilities.minImageCount;
   swapchain_info.imageFormat           = vulk_data->colour_format;
   swapchain_info.imageColorSpace       = vulk_data->swapchain_info.imageColorSpace;
   swapchain_info.imageExtent.width     = vulk_data->window_dim[0];
   swapchain_info.imageExtent.height    = vulk_data->window_dim[1];
   swapchain_info.imageArrayLayers      = vulk_data->swapchain_info.imageArrayLayers;
   swapchain_info.imageUsage            = vulk_data->swapchain_info.imageUsage;
   swapchain_info.imageSharingMode      = vulk_data->swapchain_info.imageSharingMode;
   swapchain_info.queueFamilyIndexCount = vulk_data->queue_family_count;
   swapchain_info.pQueueFamilyIndices   = vulk_data->queue_family;
   swapchain_info.preTransform          = vulk_data->swapchain_info.preTransform;
   swapchain_info.compositeAlpha        = vulk_data->swapchain_info.compositeAlpha;
   swapchain_info.presentMode           = vulk_data->swapchain_info.presentMode;
   swapchain_info.oldSwapchain          = vulk_data->swapchain;

   vulk_data->swapchain_info = swapchain_info;
   vkCreateSwapchainKHR(vulk_data->logical_device, &swapchain_info, VK_NULL_HANDLE, &vulk_data->swapchain);

   u32 image_count;
   vkGetSwapchainImagesKHR(vulk_data->logical_device, vulk_data->swapchain, &image_count, NULL);
   VkImage images[image_count];
   vkGetSwapchainImagesKHR(vulk_data->logical_device, vulk_data->swapchain, &image_count, images);

   for (u8 i = 0; i < image_count; i++)
   {
      vulk_data->images[i] = images[i];
   }
   return true;
}

u32
get_mem_type_index(VULK_DATA_INC, u32 mem_bits, VkMemoryPropertyFlags mem_flags)
{
   for (u8 i = 0; i < vulk_data->mem_props.memoryTypeCount; i++)
   {
      if (((mem_bits & (1 << i)) != 0) && (vulk_data->mem_props.memoryTypes[i].propertyFlags & mem_flags))
      {
         return i; 
      }
   }
   return 0;
}

void
resize_image_struct(VULK_DATA_INC, image_struct* image)
{
   VkMemoryRequirements mem_reqs       = {0};
   u32                  mem_type_index = 0;
   VkMemoryAllocateInfo mem_alloc_info = {0};

   image->image_info.extent.width   = vulk_data->window_dim[0];
   image->image_info.extent.height  = vulk_data->window_dim[1];

   vkCreateImage(vulk_data->logical_device, &image->image_info, NULL, &image->image);
   vkGetImageMemoryRequirements(vulk_data->logical_device, image->image, &mem_reqs);

   mem_type_index = get_mem_type_index(vulk_data, mem_reqs.memoryTypeBits, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT);
   
   mem_alloc_info.sType           = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
   mem_alloc_info.pNext           = NULL;
   mem_alloc_info.allocationSize  = mem_reqs.size;
   mem_alloc_info.memoryTypeIndex = mem_type_index;

   vkAllocateMemory(vulk_data->logical_device, &mem_alloc_info, NULL, &image->device_memory);
   vkBindImageMemory(vulk_data->logical_device, image->image, image->device_memory, 0);
   
   image->image_view_info.image = image->image;
   vkCreateImageView(vulk_data->logical_device, &image->image_view_info, NULL, &image->image_view);
}

VkFramebuffer
create_framebuffer(VULK_DATA_INC, image_struct* images, u32 image_count)
{
   VkFramebuffer           framebuffer = {0};
   VkFramebufferCreateInfo framebuffer_info = {0};
   VkImageView             image_views[image_count];

   for (u16 i = 0; i < image_count; i++)
   {
      image_views[i] = images[i].image_view;
   }
   framebuffer_info.sType           = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
   framebuffer_info.pNext           = NULL;
   framebuffer_info.renderPass      = vulk_data->render_pass;
   framebuffer_info.attachmentCount = image_count;
   framebuffer_info.pAttachments    = image_views;
   framebuffer_info.width           = vulk_data->window_dim[0];
   framebuffer_info.height          = vulk_data->window_dim[1];
   framebuffer_info.layers          = 1;

   vkCreateFramebuffer(vulk_data->logical_device, &framebuffer_info, NULL, &framebuffer);
   return framebuffer;
}

VkImageMemoryBarrier
create_image_barrier(VkImage image, VkAccessFlags src_access_mask, VkAccessFlags dst_access_mask, VkImageLayout old_layout, VkImageLayout new_layout, VkImageAspectFlags aspect_flags)
{
   VkImageMemoryBarrier image_memory_barrier = {0};

   image_memory_barrier.sType               = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
   image_memory_barrier.pNext               = NULL;
   image_memory_barrier.srcAccessMask       = src_access_mask;
   image_memory_barrier.dstAccessMask       = dst_access_mask;
   image_memory_barrier.oldLayout           = old_layout;
   image_memory_barrier.newLayout           = new_layout;
   image_memory_barrier.srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
   image_memory_barrier.dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
   image_memory_barrier.image               = image;
   image_memory_barrier.subresourceRange.baseMipLevel   = 0;
   image_memory_barrier.subresourceRange.levelCount     = VK_REMAINING_MIP_LEVELS;
   image_memory_barrier.subresourceRange.baseArrayLayer = 0;
   image_memory_barrier.subresourceRange.layerCount     = VK_REMAINING_ARRAY_LAYERS;
   if (aspect_flags != 0)
   {
      image_memory_barrier.subresourceRange.aspectMask  = aspect_flags;
   }

   return image_memory_barrier;
}

void
create_buffer(VULK_DATA_INC, struct vulk_buffer* buffer, size_t size, VkBufferUsageFlags buffer_flags, VkMemoryPropertyFlags mem_flags)
{
   VkBufferCreateInfo   buffer_info = {0};
   VkMemoryRequirements mem_reqs = {0};
   VkMemoryAllocateInfo mem_alloc_info = {0};
   u32                  mem_index = 0;
   
   buffer_info.sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
   buffer_info.pNext = NULL;
   buffer_info.size  = size;
   buffer_info.usage = buffer_flags;
   vkCreateBuffer(vulk_data->logical_device, &buffer_info, NULL, &buffer->buffer);

   vkGetBufferMemoryRequirements(vulk_data->logical_device, buffer->buffer, &mem_reqs);
   mem_index = get_mem_type_index(vulk_data, mem_reqs.memoryTypeBits, mem_flags);

   mem_alloc_info.sType           = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
   mem_alloc_info.pNext           = NULL;
   mem_alloc_info.allocationSize  = mem_reqs.size;
   mem_alloc_info.memoryTypeIndex = mem_index;

   vkAllocateMemory(vulk_data->logical_device, &mem_alloc_info, NULL, &buffer->device_memory);
   vkBindBufferMemory(vulk_data->logical_device, buffer->buffer, buffer->device_memory, 0);
   if (mem_flags & VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT)
   {
      vkMapMemory(vulk_data->logical_device, buffer->device_memory, 0, size, 0, &buffer->data);
   }
   buffer->size = size;
}
   
void
fill_buffer(VULK_DATA_INC, struct vulk_buffer scratch, struct vulk_buffer* buffer, void* data, size_t size)
{
   VkCommandBufferBeginInfo begin_info     = {0};
   VkBufferMemoryBarrier    memory_barrier = {0};
   VkSubmitInfo             submit_info    = {0};
   VkDeviceSize             device_size    = size;
   VkBufferCopy             buffer_copy    = {0};
   u8                       index          = 0; 

   memcpy(scratch.data, data, size);
   vkDeviceWaitIdle(vulk_data->logical_device);
   vkResetCommandPool(vulk_data->logical_device, vulk_data->command_pool[index], 0);

   begin_info.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
   begin_info.pNext = NULL;
   begin_info.flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT;
   vkBeginCommandBuffer(vulk_data->command_buffer[index], &begin_info);

   buffer_copy.srcOffset = 0;
   buffer_copy.dstOffset = 0;
   buffer_copy.size      = device_size;
   vkCmdCopyBuffer(vulk_data->command_buffer[index], scratch.buffer, buffer->buffer, 1, &buffer_copy);

   memory_barrier.sType               = VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER;
   memory_barrier.pNext               = NULL;
   memory_barrier.srcAccessMask       = VK_ACCESS_TRANSFER_WRITE_BIT;
   memory_barrier.dstAccessMask       = VK_ACCESS_SHADER_READ_BIT;
   memory_barrier.srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
   memory_barrier.dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
   memory_barrier.buffer              = buffer->buffer;
   memory_barrier.offset              = 0;
   memory_barrier.size                = VK_WHOLE_SIZE;
   vkCmdPipelineBarrier
      (vulk_data->command_buffer[index], 
       VK_PIPELINE_STAGE_TRANSFER_BIT, 
       VK_PIPELINE_STAGE_ALL_COMMANDS_BIT, 
       VK_DEPENDENCY_BY_REGION_BIT, 
       0, NULL, 1, 
       &memory_barrier, 
       0, NULL);
   vkEndCommandBuffer(vulk_data->command_buffer[index]);

   submit_info.sType              = VK_STRUCTURE_TYPE_SUBMIT_INFO;
   submit_info.pNext              = NULL;
   submit_info.commandBufferCount = 1;
   submit_info.pCommandBuffers    = &vulk_data->command_buffer[index];
   vkQueueSubmit(vulk_data->queue[index], 1, &submit_info, VK_NULL_HANDLE);
   vkDeviceWaitIdle(vulk_data->logical_device);
}
