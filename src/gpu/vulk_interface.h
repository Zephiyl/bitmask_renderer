/*Uses:
 * stdbool.h
 * general.h
 * ext/volk.h
 * GLFW/glfw3.h
 */

struct vulk_buffer
{
   VkBuffer   buffer;
   VkDeviceMemory device_memory;
   void*   data;
   size_t     size;
};

enum vulk_fam_index_enum 
{
      VULK_GRAPHICS_TYPE   = 0,
      VULK_COMPUTE_TYPE    = 1,
      VULK_TRANSFER_TYPE   = 2,
      VULK_SPARSE_TYPE     = 3,
};

enum vulk_shader_enum
{
      VULK_SHADER_QUAD     = 0,
      VULK_SHADER_RAYCAST  = 1,
      VULK_SHADER_LAST     = 2,
};

#define VkPhysicalDeviceMemProps VkPhysicalDeviceMemoryProperties
typedef struct vulk_data_struct
{
   VkInstance               instance;

   VkPhysicalDevice         physical_device;
   VkPhysicalDeviceMemProps mem_props;
   
   VkDevice                 logical_device;
   VkExtensionProperties*   device_extension_props;

   GLFWwindow*              window;
   s32                      window_dim[2];
   VkSurfaceKHR             surface;
   VkSurfaceCapabilitiesKHR surface_capabilities;
        
   u32  queue_family_count;
   u8   queue_family_type_count[4];
   bool queue_family_capabilities[4][4];
   u32  *queue_family;

   VkCommandPool   *command_pool;
   VkCommandBuffer *command_buffer;
   VkQueue         *queue;

   u8 transfer_index;
   u8 compute_index;

   VkPipeline            pipeline;
   VkDescriptorSetLayout descriptor_layout;
   VkPipelineLayout      pipeline_layout;
   VkRenderPass          render_pass;

   VkShaderModule        shader_quad;
   VkShaderModule        shader_raycast;

   VkSwapchainKHR           swapchain;
   VkSwapchainCreateInfoKHR swapchain_info;
   VkFramebuffer            frame_buffer;
   VkImage                  images[8];
   VkImageView              image_view;


   VkFormat           depth_format;
   VkFormat           colour_format;

   struct vulk_buffer buffer[4];
} vulk_data_struct;
#undef VkPhysicalDeviceMemProps 

#define VULK_DATA_INC vulk_data_struct* vulk_data
typedef struct image_struct
{
   VkImage               image;
   VkImageCreateInfo     image_info;
   VkImageView           image_view;
   VkImageViewCreateInfo image_view_info;
   VkDeviceMemory        device_memory;
} image_struct;

typedef struct render_data_struct
{
   u8                   graph_index;
   VkSemaphore          semaphore[2];
   VkFramebuffer        target_framebuffer;
   VkImageMemoryBarrier render_memory_barrier[2];
   u32                  render_memory_barrier_count;

   VkClearValue         clear_values[2];
   u32                  clear_values_count;
   VkViewport           viewport;
   VkRect2D             scissor;

   u32                  image_index;
   VkImageCopy          image_copy;
   VkImageMemoryBarrier image_copy_memory_barrier[2];
   u32                  image_copy_memory_barrier_count;
   VkImageMemoryBarrier present_memory_barrier;

   VkCommandBufferBeginInfo command_buffer_info;
   VkRenderPassBeginInfo    render_pass_info;
   VkPipelineStageFlags     pipe_stage_flags;
   VkSubmitInfo             submit_info;
   VkPresentInfoKHR         present_info;
} render_data_struct;

void
load_shader(VULK_DATA_INC, VkShaderModule* shader, const char* shader_path);
void
get_window_size(VULK_DATA_INC);
VkSemaphore
create_semaphore(VULK_DATA_INC);
bool
resize_swapchain(VULK_DATA_INC);
u32
get_mem_type_index(VULK_DATA_INC, uint32_t mem_bits, VkMemoryPropertyFlags mem_flags);
void
resize_image_struct(VULK_DATA_INC, image_struct* image);
VkFramebuffer
create_framebuffer(VULK_DATA_INC, image_struct* images, uint32_t image_count);
VkImageMemoryBarrier
create_image_barrier(VkImage image, VkAccessFlags src_access_mask, VkAccessFlags dst_access_mask, VkImageLayout old_layout, VkImageLayout new_layout, VkImageAspectFlags aspect_flags);
void
create_buffer(VULK_DATA_INC, struct vulk_buffer* buffer, size_t size, VkBufferUsageFlags buffer_flags, VkMemoryPropertyFlags mem_flags);
void
fill_buffer(VULK_DATA_INC, struct vulk_buffer scratch, struct vulk_buffer* buffer, void* data, size_t size);
