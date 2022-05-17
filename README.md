## Description

The above is a simple Vulkan renderer, written primarily in C11 (With a few math functions written in Fortran 08) and with GLSL shaders. It works by displaying a full screen triangle and sending out one ray for each pixel on the screen. This ray then goes through a Z-order bitmask, by way of 3D-DDA, until it leaves the 512^3 scene or hits a voxel. If it hits a voxel, that pixel gets coloured blue, adjusted for the normal of the face it hits. Otherwise, it gets coloured based on the UV coordinates of the pixel.

## Detailed Explanation

The default scene is 512^3 voxels. This is composed of 4^3(64) 'chunks', each of which then contains 16^3(4096) bitmaps, which each contain 8^3(512) voxels. Each voxel is represented by a single bit. Each bitmap additionally has a bit associated with it in the chunk struct. If this bit is zero, the bitmap is empty and should be skipped over during the raycasting stage. This setup is a modified version of [this paper](https://studenttheses.uu.nl/handle/20.500.12932/20460). This approach, with the current scene, stores ~134 million voxels in a little over 16 MiB

The rendering process makes use of a 3D-DDA algorithm. Each ray first does an intersection test with an AABB the same size as the scene. If it fails to intersect, it immediately skips trying to step through scene to avoid indexing garbage memory. If it hits the AABB, the ray position gets moved along its direction until it touches the AABB. It then steps through the first chunk, checking the (Z-order) bitmap bit corresponding to the current position. If it is 1, it starts stepping through individual voxels. If it steps through the bitmap but doesn't hit anything, it goes back to per-bitmap stepping. If the bitmap is empty, it simply skips over it. If it reaches the end of the AABB without having hit anything, it returns false.

## Building

This project has 3 dependencies: [Volk](https://github.com/zeux/volk/), Vulkan and GLFW. Volk is included within the project files, but Vulkan and GLFW must be linked from elsewhere. The provided Makefile does so automatically on linux, but I'm unsure if it works on Windows. The Makefile requires a C compiler, a Fortran compiler and a GLSL to SPV translator. It uses GCC's C and Fortran compiler and glslangValidator by default.

## Sample Output
![img1](https://repository-images.githubusercontent.com/492968298/8f751224-acad-4251-9aef-a51238368e51)
