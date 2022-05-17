## Description

The above is a simple Vulkan renderer, written primarily in C11 (With a few math functions written in Fortran 08) and with the shaders in GLSL. It works by displaying a full screen triangle and sending out one ray for each pixel on the screen. This ray then goes through a Z-order bitmask, by way of 3D-DDA, until it leaves the 512^3 'world' area or hits a voxel. If it hits a voxel, that bit of the screen gets coloured blue, adjusted for the normal of the face it hits. Otherwise, it gets coloured depending on the UV coordinates of the pixel.

## Detailed Explanation

The scene the application renders by default is 512^3 voxels. This is composed of 4^3(64) 'chunks', each of which then contains 16^3(4096) bitmaps, which each contain 8^3(512) voxels. Each voxel is represented by a single bit. Each bitmap additionally has a bit associated with it in the chunk struct. If the bit is zero, the bitmap is empty and should be skipped over during the raycasting stage. This setup is a simplified version of [this paper](https://studenttheses.uu.nl/handle/20.500.12932/20460), modified for the simpler approach taken here. This approach, with the current scene, stores ~134 million voxels in a little over 16 MiB

The rendering process itself makes use of a 3D-DDA algorithm. Each ray first does an intersection test with an AABB the same size as the scene. If it fails to intersect, it immediately skips trying to step through the actual scene for performance reasons and to avoid indexing garbage memory. If it hits the AABB, the ray position gets moved along its direction until it just barely touches the AABB. It then steps through each individual brick, checking the (Z-order) bitmap bits for if the bitmap has anything in it. If it does, it starts stepping through individual voxels. If it leaves the bitmap's area, it goes back to per-bitmap stepping. If the bitmap is empty, it simply skips over it. If it reaches the end of the AABB without having hit anything, it returns false.

The ray steps in one direction at a time, incrementing a vec's member corresponding to the axis it stepped in each time. The next axis for stepping is whichever of the aforementioned vecs members is lowest. This way it does steps per cubic volume, but none of the ray's direction is lost. The normal is simply the inverse of the ray's current stepping direction.

## Building

This project has 3 dependencies: [Volk](https://github.com/zeux/volk/), Vulkan and GLFW. Volk is included within the project files, but Vulkan and GLFW must be linked from elsewhere. The provided Makefile does so automatically, but I'm unsure of if it works on Windows. Given the small dependency list and small project size it shouldn't be difficult to fix the Makefile for use on Windows. The Makefile requires a C compiler, a Fortran compiler and a GLSL to SPV translator. It uses GCC's C and Fortran compiler and glslangValidator by default.

## Sample Output
![img1](https://repository-images.githubusercontent.com/492968298/8f751224-acad-4251-9aef-a51238368e51)
