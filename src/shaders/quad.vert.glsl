#version 460

#extension GL_ARB_shader_draw_parameters: require

layout (location = 0) out vec2 uv;

void 
main(void)
{
   uv          = vec2((gl_VertexIndex << 1) & 2, gl_VertexIndex & 2);
   gl_Position = vec4(uv * 2.0f + -1.0f, 1.0f, 1.0f);
}
