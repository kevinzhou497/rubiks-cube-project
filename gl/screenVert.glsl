#version 330 core
layout (location = 0) in vec2 pos;
layout (location = 1) in vec2 off;

uniform mat4 model;
uniform bool instanced;
out vec2 texCoords;
flat out int useColors;
flat out int id;

// gets a vector in range [-1, 1] from a vector in range [0, 1]
vec2 texCoordsToNdc(vec2 p);

void main () {
    vec2 p = texCoordsToNdc(pos);   
    gl_Position = model * vec4(p, 0.0, 1.0);
    if (instanced) gl_Position.xy += off;
    texCoords = pos;
    useColors = instanced ? 1 : 0;
    id = gl_InstanceID;
}

vec2 texCoordsToNdc(vec2 p) {
    return p * 2.0 - vec2(1.0);
}