#version 330 core
layout (location = 0) in vec3 pos;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec2 texCoords;
layout (location = 3) in vec3 instanceOffset;

uniform mat4 projection;
uniform mat4 view;
uniform mat4 model;
// bottom, top, right, left, front, back
uniform mat4 sliceMatrices[6] = mat4[6](
    mat4(1.0), mat4(1.0), mat4(1.0), mat4(1.0), mat4(1.0), mat4(1.0)
);
// Invariant: only the rotating face is set, the other matrices are identity

out vec3 fragPos;
out vec3 fragNorm;
out vec2 fragCoords;
flat out int instanceId;

mat4 getLocalTransform();

void main() {
    vec3 position = instanceOffset + pos;
    mat4 worldSpace = model * getLocalTransform();
    fragPos = vec3(worldSpace * vec4(position, 1.0));
    fragCoords = texCoords;
    fragNorm = mat3(transpose(inverse(view * worldSpace))) * normal;
    instanceId = gl_InstanceID;
    gl_Position = projection * view * worldSpace * vec4(position, 1.0);
}

mat4 getLocalTransform() {    
    mat4 trans[3] = mat4[3](
        mat4(1.0), mat4(1.0), mat4(1.0)
    );
    int topBottom = gl_InstanceID / 9;
    if(topBottom == 0 || topBottom == 2)
        trans[0] = sliceMatrices[(topBottom + 1) % 3];
    int leftRight = gl_InstanceID % 3;
    if(leftRight == 0 || leftRight == 2)
        trans[1] = sliceMatrices[max(0, leftRight - 1) + 2];
    int frontBack = gl_InstanceID % 9;
    if(frontBack < 3 || frontBack >= 6)
        trans[2] = sliceMatrices[max(0, frontBack / 3 - 1) + 4];

    return trans[0] * trans[1] * trans[2];
}