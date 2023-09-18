#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>

#include "ext/volk.h"
#include <GLFW/glfw3.h>

#include "util/util_typedefs.h"

#include "gpu/vulk_interface.h"

#include "init.h"

void
init_glfw(VULK_DATA_INC, const char* name)
{
   glfwInit();
   glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
   vulk_data->window = glfwCreateWindow
      (1280, 720, //x, y
       name,
       NULL, NULL);
   glfwGetWindowSize(vulk_data->window, &vulk_data->window_dim[0], &vulk_data->window_dim[1]);
}

void
init_vulkan(VULK_DATA_INC, const char* name)
{  //INIT VOLK
   volkInitialize();

   //INIT INSTANCE
   u32 instance_extension_count = 0;
   const char** instance_extension_names = glfwGetRequiredInstanceExtensions(&instance_extension_count);

   VkApplicationInfo app_info  = {0};
   app_info.sType              = VK_STRUCTURE_TYPE_APPLICATION_INFO;
   app_info.pNext              = NULL;
   app_info.pApplicationName   = name;
   app_info.applicationVersion = 1;
   app_info.pEngineName        = name;
   app_info.engineVersion      = 1;
   app_info.apiVersion         = VK_API_VERSION_1_1; 

   VkInstanceCreateInfo instance_info    = {0};
   instance_info.sType                   = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
   instance_info.pNext                   = NULL;
   instance_info.pApplicationInfo        = &app_info;
   instance_info.enabledLayerCount       = 0;
   instance_info.enabledExtensionCount   = instance_extension_count;
   instance_info.ppEnabledExtensionNames = instance_extension_names;

   vkCreateInstance(&instance_info, NULL, &vulk_data->instance);
   volkLoadInstance(vulk_data->instance);

   //INIT PHYSICAL DEVICE
   VkPhysicalDevice physical_devices[16] = {0};
   u32 physical_device_count = 16;
   vkEnumeratePhysicalDevices(vulk_data->instance, &physical_device_count, physical_devices);

   VkPhysicalDeviceProperties device_props;
   bool has_discrete = false;
   for (u32 i = 0; i < physical_device_count; ++i) {
      vkGetPhysicalDeviceProperties(physical_devices[i], &device_props);
      if (device_props.deviceType == VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU)
      {
         printf("Chose GPU named: "); 
         printf(device_props.deviceName);
         printf("\n");
         vulk_data->physical_device = physical_devices[i];
         goto PHYSICAL_DEVICE_END;
      }
   }
   for (u32 i = 0; i < physical_device_count; ++i) {
      vkGetPhysicalDeviceProperties(physical_devices[i], &device_props);
      if (device_props.deviceType == VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU)
      {
         printf("Chose GPU named: "); 
         printf(device_props.deviceName);
         printf("\n");
         vulk_data->physical_device = physical_devices[i];
      }
   }
PHYSICAL_DEVICE_END:
   vkGetPhysicalDeviceMemoryProperties(vulk_data->physical_device, &vulk_data->mem_props);

   //INIT QUEUE FAMILIES
   vulk_data->queue_family_count = 0;

   vkGetPhysicalDeviceQueueFamilyProperties(vulk_data->physical_device, &vulk_data->queue_family_count, NULL);

   VkQueueFamilyProperties queue_fam_props[vulk_data->queue_family_count];
   vkGetPhysicalDeviceQueueFamilyProperties(vulk_data->physical_device, &vulk_data->queue_family_count, queue_fam_props);
   
   if (vulk_data->queue_family_count > 4) {
      vulk_data->queue_family_count = 4;
   }

   vulk_data->queue_family    = malloc(sizeof(u32)             * vulk_data->queue_family_count);
   vulk_data->command_pool    = malloc(sizeof(VkCommandPool)   * vulk_data->queue_family_count);
   vulk_data->command_buffer  = malloc(sizeof(VkCommandBuffer) * vulk_data->queue_family_count);
   vulk_data->queue           = malloc(sizeof(VkQueue)         * vulk_data->queue_family_count);

   vulk_data->queue_family_type_count[VULK_GRAPHICS_TYPE] = 0;
   vulk_data->queue_family_type_count[VULK_COMPUTE_TYPE]  = 0;
   vulk_data->queue_family_type_count[VULK_TRANSFER_TYPE] = 0;
   vulk_data->queue_family_type_count[VULK_SPARSE_TYPE]   = 0;

   for (s64 i = 0; i < vulk_data->queue_family_count; ++i)
   {
      vulk_data->queue_family[i] = i;
      if (queue_fam_props[i].queueFlags & VK_QUEUE_GRAPHICS_BIT) 
      {
         vulk_data->queue_family_capabilities[i][VULK_GRAPHICS_TYPE] = true;
    
         vulk_data->queue_family_type_count[VULK_GRAPHICS_TYPE]++;
      }
      if (queue_fam_props[i].queueFlags & VK_QUEUE_COMPUTE_BIT) 
      {
         vulk_data->queue_family_capabilities[i][VULK_COMPUTE_TYPE] = true;

         vulk_data->queue_family_type_count[VULK_COMPUTE_TYPE]++;
      }
      if (queue_fam_props[i].queueFlags & VK_QUEUE_TRANSFER_BIT) 
      {
         vulk_data->queue_family_capabilities[i][VULK_TRANSFER_TYPE] = true;

         vulk_data->queue_family_type_count[VULK_TRANSFER_TYPE]++;
      }
      if (queue_fam_props[i].queueFlags & VK_QUEUE_SPARSE_BINDING_BIT) 
      {
         vulk_data->queue_family_capabilities[i][VULK_SPARSE_TYPE] = true;

         vulk_data->queue_family_type_count[VULK_SPARSE_TYPE]++;
      }
   } //INIT LOGICAL DEVICE
   float queue_priorities = 1.0f;

   u32 extension_count = 0;
   vkEnumerateDeviceExtensionProperties(vulk_data->physical_device, 0, &extension_count, NULL);

   vulk_data->device_extension_props   = malloc(sizeof(VkExtensionProperties) * extension_count);
   vkEnumerateDeviceExtensionProperties(vulk_data->physical_device, NULL, &extension_count, vulk_data->device_extension_props);

   const char *extension_names[] = {"VK_KHR_swapchain", "VK_KHR_push_descriptor"};

   VkDeviceQueueCreateInfo queue_info[vulk_data->queue_family_count];
   for (u8 i = 0; i < vulk_data->queue_family_count; i++) 
   {
      queue_info[i].sType            = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
      queue_info[i].pNext            = NULL;
      queue_info[i].queueFamilyIndex = i;
      queue_info[i].flags            = 0;
      queue_info[i].queueCount       = 1;
      queue_info[i].pQueuePriorities = &queue_priorities;
   }

   VkDeviceCreateInfo device_info = {0};
   device_info.sType                         = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
   device_info.pNext                         = NULL;
   device_info.queueCreateInfoCount          = vulk_data->queue_family_count;
   device_info.pQueueCreateInfos             = queue_info;
   device_info.enabledExtensionCount         = 2;
   device_info.ppEnabledExtensionNames       = extension_names;

   VkPhysicalDeviceFeatures2 device_features = {0};
   device_features.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2;
   device_features.pNext = NULL;

   vkGetPhysicalDeviceFeatures2(vulk_data->physical_device, &device_features);
   device_info.pEnabledFeatures = &device_features.features;

   vkCreateDevice(vulk_data->physical_device, &device_info, 0, &vulk_data->logical_device);
   //INIT COMMANDBUFFERS
   for (u8 i = 0; i < vulk_data->queue_family_count; i++) 
   {
      vulk_data->queue[i] = NULL;

      VkCommandBufferAllocateInfo alloc_info = {0};
      alloc_info.sType              = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
      alloc_info.pNext              = NULL;
      alloc_info.level              = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
      alloc_info.commandBufferCount = 1;
   
      VkCommandPoolCreateInfo command_pool_info = {0};
      command_pool_info.sType             = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
      command_pool_info.pNext             = NULL;
      command_pool_info.flags             = VK_COMMAND_POOL_CREATE_TRANSIENT_BIT;
      command_pool_info.queueFamilyIndex  = i;
   
      vkGetDeviceQueue(vulk_data->logical_device, i, 0, &vulk_data->queue[i]);

      vkCreateCommandPool(vulk_data->logical_device, &command_pool_info, NULL, &vulk_data->command_pool[i]);
      alloc_info.commandPool = vulk_data->command_pool[i];

      vkAllocateCommandBuffers(vulk_data->logical_device, &alloc_info, &vulk_data->command_buffer[i]);
   }
}

void
init_surface(VULK_DATA_INC)
{
   VkBool32 surface_supported = VK_FALSE;
   glfwCreateWindowSurface
      (vulk_data->instance,
      vulk_data->window,
      NULL,
      &vulk_data->surface);

   for (u32 i = 0; i < vulk_data->queue_family_count; i++)
   { 
      if (vulk_data->queue_family_capabilities[i][VULK_GRAPHICS_TYPE]) 
      {   
         vkGetPhysicalDeviceSurfaceSupportKHR(vulk_data->physical_device, i, vulk_data->surface, &surface_supported);
      }

      if (surface_supported == VK_TRUE) 
      {
         vkGetPhysicalDeviceSurfaceCapabilitiesKHR(vulk_data->physical_device, vulk_data->surface, &vulk_data->surface_capabilities);
         return;
      }
   }
   printf("Error! No surface support."); 
}

void
init_shaders(VULK_DATA_INC)
{
   load_shader(vulk_data, &vulk_data->shader_quad,    ".spv/quad.vert.spv");
   load_shader(vulk_data, &vulk_data->shader_raycast, ".spv/raycast.frag.spv");
}

void
init_render_pass(VULK_DATA_INC)
{  
   u32 colour_format_count = 0;
   vkGetPhysicalDeviceSurfaceFormatsKHR(vulk_data->physical_device, vulk_data->surface, &colour_format_count, NULL);
   VkSurfaceFormatKHR surface_formats[colour_format_count];
   vkGetPhysicalDeviceSurfaceFormatsKHR(vulk_data->physical_device, vulk_data->surface, &colour_format_count, surface_formats);

   if ((colour_format_count == 1) && (surface_formats[0].format == VK_FORMAT_UNDEFINED))
   {
      vulk_data->colour_format = VK_FORMAT_R8G8B8A8_UNORM;
   } else 
   {
      for (u16 i = 0; i < colour_format_count; i++)
      {
         if ((surface_formats[i].format == VK_FORMAT_R8G8B8A8_UNORM) || (surface_formats[i].format = VK_FORMAT_B8G8R8A8_UNORM))
         {
            vulk_data->colour_format = surface_formats[i].format;
         }
      }
   }
   vulk_data->depth_format = VK_FORMAT_D32_SFLOAT;

   VkAttachmentReference colour_attach_ref = {0, VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL};
   VkAttachmentReference depth_attach_ref  = {1, VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL};

   //Render pass init
   VkSubpassDescription subpass = {0};
   subpass.pipelineBindPoint       = VK_PIPELINE_BIND_POINT_GRAPHICS;
   subpass.colorAttachmentCount    = 1;
   subpass.pColorAttachments       = &colour_attach_ref;
   subpass.pDepthStencilAttachment = &depth_attach_ref;
   
   u32 attachments_count = 2;
   VkAttachmentDescription attachments[attachments_count];
   attachments[0].flags     = 0;
   attachments[0].format    = vulk_data->colour_format;
   attachments[0].samples   = VK_SAMPLE_COUNT_1_BIT;
   attachments[0].loadOp    = VK_ATTACHMENT_LOAD_OP_CLEAR;
   attachments[0].storeOp   = VK_ATTACHMENT_STORE_OP_STORE;
   attachments[0].stencilLoadOp   = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
   attachments[0].stencilStoreOp  = VK_ATTACHMENT_STORE_OP_DONT_CARE;
   attachments[0].initialLayout   = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
   attachments[0].finalLayout     = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
   
   attachments[1].flags     = 0;
   attachments[1].format    = vulk_data->depth_format;
   attachments[1].samples   = VK_SAMPLE_COUNT_1_BIT;
   attachments[1].loadOp    = VK_ATTACHMENT_LOAD_OP_CLEAR;
   attachments[1].storeOp   = VK_ATTACHMENT_STORE_OP_DONT_CARE;
   attachments[1].stencilLoadOp   = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
   attachments[1].stencilStoreOp  = VK_ATTACHMENT_STORE_OP_DONT_CARE;
   attachments[1].initialLayout   = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;
   attachments[1].finalLayout     = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;

   VkRenderPassCreateInfo render_pass_info = {0};
   render_pass_info.sType            = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;
   render_pass_info.pNext            = NULL;
   render_pass_info.attachmentCount  = attachments_count;
   render_pass_info.pAttachments     = attachments;
   render_pass_info.subpassCount     = 1;
   render_pass_info.pSubpasses       = &subpass;

   vkCreateRenderPass(vulk_data->logical_device, &render_pass_info, NULL, &vulk_data->render_pass);
}

void
init_descriptor_set(VULK_DATA_INC)
{
   VkDescriptorSetLayoutBinding desc_set_bindings[2] = {0};
   desc_set_bindings[0].binding         = 0;
   desc_set_bindings[0].descriptorType  = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
   desc_set_bindings[0].descriptorCount = 1;
   desc_set_bindings[0].stageFlags      = VK_SHADER_STAGE_FRAGMENT_BIT;

   desc_set_bindings[1].binding         = 1;
   desc_set_bindings[1].descriptorType  = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
   desc_set_bindings[1].descriptorCount = 1;
   desc_set_bindings[1].stageFlags      = VK_SHADER_STAGE_FRAGMENT_BIT;

   VkDescriptorSetLayoutCreateInfo desc_set_info = {0};
   desc_set_info.sType        = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
   desc_set_info.pNext        = NULL;
   desc_set_info.flags        = VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR;
   desc_set_info.bindingCount = 2;
   desc_set_info.pBindings    = desc_set_bindings;

   vkCreateDescriptorSetLayout(vulk_data->logical_device, &desc_set_info, NULL, &vulk_data->descriptor_layout);
}

void
init_pipeline(VULK_DATA_INC)
{
   VkPipelineLayoutCreateInfo layout_info = {0};
   layout_info.sType          = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
   layout_info.pNext          = NULL;
   layout_info.setLayoutCount = 1;
   layout_info.pSetLayouts    = &vulk_data->descriptor_layout;
   vkCreatePipelineLayout(vulk_data->logical_device, &layout_info, NULL, &vulk_data->pipeline_layout);

   VkGraphicsPipelineCreateInfo pipe_info = {0};
   pipe_info.sType = VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO;
   pipe_info.pNext = NULL;

   VkPipelineShaderStageCreateInfo shader_info[2] = {0};
   shader_info[0].sType          = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
   shader_info[0].pNext          = NULL;
   shader_info[0].flags          = 0;
   shader_info[0].stage          = VK_SHADER_STAGE_VERTEX_BIT;
   shader_info[0].module         = vulk_data->shader_quad;
   shader_info[0].pName          = "main";
   shader_info[0].pSpecializationInfo = NULL;

   shader_info[1].sType          = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
   shader_info[1].pNext          = NULL;
   shader_info[1].flags          = 0;
   shader_info[1].stage          = VK_SHADER_STAGE_FRAGMENT_BIT;
   shader_info[1].module         = vulk_data->shader_raycast;
   shader_info[1].pName          = "main";
   shader_info[1].pSpecializationInfo = NULL;

   pipe_info.stageCount = 2;
   pipe_info.pStages = shader_info;

   VkPipelineVertexInputStateCreateInfo vertex_info = {0};
   vertex_info.sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
   vertex_info.pNext = NULL;
   vertex_info.vertexAttributeDescriptionCount = 0; 
   vertex_info.pVertexAttributeDescriptions    = NULL;
   vertex_info.vertexBindingDescriptionCount   = 0;
   vertex_info.pVertexBindingDescriptions      = NULL;
   pipe_info.pVertexInputState                 = &vertex_info;

   VkPipelineInputAssemblyStateCreateInfo assembly_info = {0};
   assembly_info.sType           = VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO;
   assembly_info.pNext           = NULL;
   assembly_info.topology        = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
   pipe_info.pInputAssemblyState = &assembly_info;

   VkPipelineViewportStateCreateInfo viewport_info = {0};
   viewport_info.sType         = VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO;
   viewport_info.pNext         = NULL;
   viewport_info.viewportCount = 1;
   viewport_info.scissorCount  = 1;
   pipe_info.pViewportState    = &viewport_info;

   VkPipelineRasterizationStateCreateInfo rasterization_info = {0};
   rasterization_info.sType         = VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO;
   rasterization_info.pNext         = NULL;
   rasterization_info.lineWidth     = 1.f;
   rasterization_info.polygonMode   = VK_POLYGON_MODE_FILL;
   pipe_info.pRasterizationState    = &rasterization_info;

   VkPipelineMultisampleStateCreateInfo multisample_info = {0};
   multisample_info.sType                = VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO;
   multisample_info.pNext                = NULL;
   multisample_info.rasterizationSamples = VK_SAMPLE_COUNT_1_BIT;
   pipe_info.pMultisampleState           = &multisample_info;

   VkPipelineDepthStencilStateCreateInfo depth_info = {0};
   depth_info.sType              = VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO;
   depth_info.pNext              = NULL;
   pipe_info.pDepthStencilState = &depth_info;

   VkPipelineColorBlendAttachmentState blend_attach_info = {0};
   blend_attach_info.colorWriteMask               = VK_COLOR_COMPONENT_R_BIT | VK_COLOR_COMPONENT_G_BIT | VK_COLOR_COMPONENT_B_BIT | VK_COLOR_COMPONENT_A_BIT;
   VkPipelineColorBlendStateCreateInfo blend_info = {0};
   blend_info.sType           = VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO;
   blend_info.pNext           = NULL;
   blend_info.attachmentCount = 1;
   blend_info.pAttachments    = &blend_attach_info;
   pipe_info.pColorBlendState = &blend_info;

   u32 dynamic_state_count = 2;
   VkDynamicState dynamic_states[dynamic_state_count];
   dynamic_states[0] = VK_DYNAMIC_STATE_VIEWPORT; 
   dynamic_states[1] = VK_DYNAMIC_STATE_SCISSOR;

   VkPipelineDynamicStateCreateInfo dynamic_info = {0};
   dynamic_info.sType             = VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO;
   dynamic_info.pNext             = NULL;
   dynamic_info.dynamicStateCount = dynamic_state_count;
   dynamic_info.pDynamicStates    = dynamic_states;
   pipe_info.pDynamicState        = &dynamic_info;

   pipe_info.layout               = vulk_data->pipeline_layout;
   pipe_info.renderPass           = vulk_data->render_pass;

   vkCreateGraphicsPipelines(vulk_data->logical_device, VK_NULL_HANDLE, 1, &pipe_info, NULL, &vulk_data->pipeline);
}

void
init_swapchain(VULK_DATA_INC)
{
   VkCompositeAlphaFlagBitsKHR composite_alpha_bit = VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR;
   get_window_size(vulk_data);

   VkSwapchainCreateInfoKHR swapchain_info = {0};
   swapchain_info.sType                 = VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR;
   swapchain_info.pNext                 = NULL;
   swapchain_info.surface               = vulk_data->surface;
   swapchain_info.minImageCount         = vulk_data->surface_capabilities.minImageCount;
   swapchain_info.imageFormat           = vulk_data->colour_format;
   swapchain_info.imageColorSpace       = VK_COLOR_SPACE_SRGB_NONLINEAR_KHR;
   swapchain_info.imageExtent.width     = vulk_data->window_dim[0];
   swapchain_info.imageExtent.height    = vulk_data->window_dim[1];
   swapchain_info.imageArrayLayers      = 1;
   swapchain_info.imageUsage            = VK_IMAGE_USAGE_TRANSFER_DST_BIT;
   swapchain_info.imageSharingMode      = VK_SHARING_MODE_CONCURRENT;
   swapchain_info.queueFamilyIndexCount = vulk_data->queue_family_count;
   swapchain_info.pQueueFamilyIndices   = vulk_data->queue_family;
   swapchain_info.preTransform          = VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR;
   swapchain_info.compositeAlpha        = composite_alpha_bit;
   swapchain_info.presentMode           = VK_PRESENT_MODE_FIFO_KHR;
   swapchain_info.oldSwapchain          = VK_NULL_HANDLE;

   vulk_data->swapchain_info = swapchain_info;

   vkCreateSwapchainKHR(vulk_data->logical_device, &swapchain_info, VK_NULL_HANDLE, &vulk_data->swapchain);

   u32 image_count = 0;
   vkGetSwapchainImagesKHR(vulk_data->logical_device, vulk_data->swapchain, &image_count, NULL);
   VkImage images[image_count];
   vkGetSwapchainImagesKHR(vulk_data->logical_device, vulk_data->swapchain, &image_count, images);

   for (u8 i = 0; i < image_count; i++)
   {
      vulk_data->images[i] = images[i];
   }
}

void
init_image_struct(VULK_DATA_INC, image_struct* image, VkFormat format, VkImageUsageFlags image_flags, VkImageAspectFlags aspect_flags)
{
   VkMemoryRequirements mem_reqs       = {0};
   u32                  mem_type_index = 0;
   VkMemoryAllocateInfo mem_alloc_info = {0};

   image->image_info.sType         = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
   image->image_info.pNext         = NULL;
   image->image_info.flags         = 0;
   image->image_info.imageType     = VK_IMAGE_TYPE_2D;
   image->image_info.format        = format;
   image->image_info.extent.width  = vulk_data->window_dim[0];
   image->image_info.extent.height = vulk_data->window_dim[1];
   image->image_info.extent.depth  = 1;
   image->image_info.mipLevels     = 1;
   image->image_info.arrayLayers   = 1;
   image->image_info.samples       = VK_SAMPLE_COUNT_1_BIT;
   image->image_info.tiling        = VK_IMAGE_TILING_OPTIMAL;
   image->image_info.usage         = image_flags;
   image->image_info.sharingMode   = VK_SHARING_MODE_EXCLUSIVE;
   image->image_info.initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;

   vkCreateImage(vulk_data->logical_device, &image->image_info, NULL, &image->image);
   vkGetImageMemoryRequirements(vulk_data->logical_device, image->image, &mem_reqs);

   mem_type_index = get_mem_type_index(vulk_data, mem_reqs.memoryTypeBits, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT);
   
   mem_alloc_info.sType           = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
   mem_alloc_info.pNext           = NULL;
   mem_alloc_info.allocationSize  = mem_reqs.size;
   mem_alloc_info.memoryTypeIndex = mem_type_index;

   vkAllocateMemory(vulk_data->logical_device, &mem_alloc_info, NULL, &image->device_memory);
   vkBindImageMemory(vulk_data->logical_device, image->image, image->device_memory, 0);
   
   image->image_view_info.sType       = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
   image->image_view_info.pNext       = NULL;
   image->image_view_info.flags       = 0;
   image->image_view_info.image       = image->image;
   //image->image_view_info.viewType    = VK_IMAGE_TYPE_2D;
   image->image_view_info.viewType    = VK_IMAGE_VIEW_TYPE_2D;
   image->image_view_info.format      = format;
   image->image_view_info.components.r   = 0;
   image->image_view_info.components.g   = 0;
   image->image_view_info.components.b   = 0;
   image->image_view_info.components.a   = 0;
   image->image_view_info.subresourceRange.levelCount     = 1;
   image->image_view_info.subresourceRange.baseMipLevel   = 0;
   image->image_view_info.subresourceRange.layerCount     = 1;
   image->image_view_info.subresourceRange.baseArrayLayer = 0;
   image->image_view_info.subresourceRange.aspectMask     = aspect_flags;

   vkCreateImageView(vulk_data->logical_device, &image->image_view_info, NULL, &image->image_view);
}

void
init_render_data(VULK_DATA_INC, render_data_struct* render_data, image_struct* images)
{
   for (u8 i = 0; i < vulk_data->queue_family_count; i++) 
   {
      if (vulk_data->queue_family_capabilities[i][VULK_GRAPHICS_TYPE] == true)
      {
         render_data->graph_index = i;
         break;
      }
   }

   render_data->semaphore[0] = create_semaphore(vulk_data);
   render_data->semaphore[1] = create_semaphore(vulk_data);

   render_data->image_index = 0;
   render_data->command_buffer_info.sType      = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
   render_data->command_buffer_info.pNext      = NULL;
   render_data->command_buffer_info.flags      = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT;
   render_data->command_buffer_info.pInheritanceInfo = NULL;

   render_data->clear_values[0].color.float32[0] = 44.f / 255;
   render_data->clear_values[0].color.float32[1] = 10.f / 255;
   render_data->clear_values[0].color.float32[2] = 36.f / 255;
   render_data->clear_values[0].color.float32[3] = 255;
   render_data->clear_values[1].depthStencil.depth   = 0.0f;
   render_data->clear_values[1].depthStencil.stencil = 0;

   render_data->render_pass_info.sType      = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
   render_data->render_pass_info.pNext      = NULL;
   render_data->render_pass_info.renderPass = vulk_data->render_pass;
   render_data->render_pass_info.clearValueCount = render_data->clear_values_count;
   render_data->render_pass_info.pClearValues    = render_data->clear_values;
   render_data->render_pass_info.renderArea.extent.width  = 0;
   render_data->render_pass_info.renderArea.extent.height = 0;
   render_data->render_pass_info.renderArea.offset.x      = 0;
   render_data->render_pass_info.renderArea.offset.y      = 0;

   render_data->viewport.x        = 0;
   render_data->viewport.minDepth = 0;
   render_data->viewport.maxDepth = 1;
   render_data->scissor.offset.x  = 0;
   render_data->scissor.offset.y  = 0;

   render_data->image_copy.srcSubresource.aspectMask     = VK_IMAGE_ASPECT_COLOR_BIT;
   render_data->image_copy.srcSubresource.baseArrayLayer = 0;
   render_data->image_copy.srcSubresource.layerCount     = 1;
   render_data->image_copy.srcSubresource.mipLevel       = 0;
   render_data->image_copy.dstSubresource.aspectMask     = VK_IMAGE_ASPECT_COLOR_BIT;
   render_data->image_copy.dstSubresource.baseArrayLayer = 0;
   render_data->image_copy.dstSubresource.layerCount     = 1;
   render_data->image_copy.dstSubresource.mipLevel       = 0;
   render_data->image_copy.srcOffset.x   = 0;
   render_data->image_copy.srcOffset.y   = 0;
   render_data->image_copy.srcOffset.z   = 0;
   render_data->image_copy.dstOffset.x   = 0;
   render_data->image_copy.dstOffset.y   = 0;
   render_data->image_copy.dstOffset.z   = 0;
   render_data->image_copy.extent.width  = vulk_data->window_dim[0];
   render_data->image_copy.extent.height = vulk_data->window_dim[1];
   render_data->image_copy.extent.depth  = 1;

   render_data->pipe_stage_flags  = VK_PIPELINE_STAGE_TRANSFER_BIT;
   render_data->submit_info.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO;
   render_data->submit_info.pNext = NULL;
   render_data->submit_info.waitSemaphoreCount   = 1;
   render_data->submit_info.pWaitSemaphores      = &render_data->semaphore[0];
   render_data->submit_info.pWaitDstStageMask    = &render_data->pipe_stage_flags;
   render_data->submit_info.commandBufferCount   = 1;
   render_data->submit_info.pCommandBuffers      = &vulk_data->command_buffer[render_data->graph_index];
   render_data->submit_info.signalSemaphoreCount = 1;
   render_data->submit_info.pSignalSemaphores    = &render_data->semaphore[1];

   render_data->present_info.sType              = VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;
   render_data->present_info.pNext              = NULL;
   render_data->present_info.waitSemaphoreCount = 1;
   render_data->present_info.pWaitSemaphores    = &render_data->semaphore[1];
   render_data->present_info.swapchainCount     = 1;
   render_data->present_info.pSwapchains        = &vulk_data->swapchain;
   render_data->present_info.pImageIndices      = &render_data->image_index;
   render_data->present_info.pResults           = NULL;

   render_data->target_framebuffer = create_framebuffer(vulk_data, images, 2);
}

void
init_misc_indices(VULK_DATA_INC)
{
   bool transfer_found = false;
   bool compute_found  = false;
   for (u8 i = vulk_data->queue_family_count - 1; i != 0; i++)
   {
      if (vulk_data->queue_family_capabilities[i][VULK_TRANSFER_TYPE] && !transfer_found)
      {
         vulk_data->transfer_index = i;
         transfer_found = true;
      }
      if (vulk_data->queue_family_capabilities[i][VULK_COMPUTE_TYPE] && !compute_found)
      {
         vulk_data->compute_index = i;
         compute_found = true;
      }
   }
}
