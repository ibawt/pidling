#version 120
attribute vec2 a_position;
attribute vec2 a_texCoord0;
attribute vec2 transform;
attribute vec2 translation;
attribute float opacity;
varying float out_opacity;
uniform mat4 u_projTrans;
varying vec2 v_texCoords;
void main()
{
  mat4 t = mat4( cos( transform.x ), -sin( transform.x ), 0.0, 0.0,
                 sin( transform.x ),  cos( transform.x ), 0.0, 0.0,
                 0.0,                 0.0,                1.0, 0.0,
                 translation.x,       translation.y,      0.0, 1.0 );
  v_texCoords = a_texCoord0;
  out_opacity = opacity;
  gl_Position =  u_projTrans * t * vec4(transform.y * a_position.x, transform.y *a_position.y,0.0,1.0) ;
}
