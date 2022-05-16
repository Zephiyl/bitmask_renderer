/*Uses:
 * ext/volk.h
 * GLFW/glfw3.h
 * util/typedefs.h
 * gpu/vulk_interface.h
 */

void	 
init_element_data();
void
init_glfw(VULK_DATA_INC, const char* name);
void
init_vulkan(VULK_DATA_INC, const char* name);
void
init_surface(VULK_DATA_INC);

void
init_shaders(VULK_DATA_INC);
void
init_render_pass(VULK_DATA_INC);
void
init_descriptor_set(VULK_DATA_INC);
void
init_pipeline(VULK_DATA_INC);

void
init_swapchain(VULK_DATA_INC);
void
init_image_struct(VULK_DATA_INC, image_struct* image, VkFormat format, VkImageUsageFlags image_flags, VkImageAspectFlags aspect_flags);
void
init_render_data(VULK_DATA_INC, render_data_struct* render_data, image_struct* images);

void
init_misc_indices(VULK_DATA_INC);
