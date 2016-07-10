#version 120
varying vec2 v_texCoords;
varying float out_opacity;
uniform sampler2D u_texture;

void main()
{
  gl_FragColor = texture2D(u_texture, v_texCoords) * vec4(1,1,1, out_opacity);
}
