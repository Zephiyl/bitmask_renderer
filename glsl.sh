
# Shader compilation seperate from the Makefile for sanity's sake. May change later
glslangValidator -V src/shaders/quad.vert.glsl			-o spv/quad.vert.spv
glslangValidator -V src/shaders/raycast.frag.glsl		-o spv/raycast.frag.spv
