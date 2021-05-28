#version 330 core

uniform vec3 eye;
// Organized top, bottom, right, left, front, back (0, 0) to (2, 2)
uniform int tileColors[54]; 

out vec4 fragColor;

in vec3 fragPos;
in vec3 fragNorm;
in vec2 fragCoords;
flat in int instanceId;

vec3 addDirLighting(vec3 diffuseColor, vec3 lightPos);
vec3 getColor(vec3 diffuseColor);
vec3 getTileColor();
int getBtmPos();
int getLeftPos();
int getRightPos();
int getTopPos();
int getFrontPos();
int getBackPos();

void main() {
    vec3 color = getColor(getTileColor());
    const vec3 lightPos = vec3(0, 5, 5);
    vec3 colorRgb = addDirLighting(color, lightPos);

   fragColor = vec4(colorRgb, 1.0);
}

vec3 getTileColor() {

    // top, bottom, right, left, front, back
    // white, yellow, red, organge, green, blue
    const vec3 faceColors[7] = vec3[7](
        vec3(1.0),
        vec3(1.0, 1.0, 0.0),      
        vec3(1.0, 0.0, 0.0),
        vec3(1.0, .65, 0.0),
        vec3(0.0, 1.0, 0.0),
        vec3(0.0, 0.0, 1.0),
        vec3(0.0)

    );
    const int black_index = 6;
    int idx = 0;
    switch(gl_PrimitiveID / 2) {
        case 5: //top
            idx = getTopPos();
            break;
        case 4: //bottom
            idx = 9 + getBtmPos();
            break;
        case 2: //right
            idx = 2 * 9 + getRightPos();
            // offset (2 * 9) + row * 3 + column
            break;
        case 3: //left
            idx = 3 * 9 + getLeftPos();
            break;
        case 0: //front
            idx = 4 * 9 + getFrontPos();
            break;
        case 1: //back
            idx = 5 * 9 + getBackPos();
            break;
    }
    int colorNum = idx > 53 ? black_index : tileColors[max(idx, 0)];
    return faceColors[colorNum];
    //return vec3(float(instanceId) / 27.0);

}

vec3 addDirLighting(vec3 diffuseColor, vec3 lightPos) {
    vec3 normal = normalize(fragNorm);
    vec3 viewDir = normalize(eye - fragPos);
    vec3 lightDir = normalize(lightPos - fragPos);
    const float shininess = 8.0;
    const float ambientFac = 0.4;
    const vec3 lightColor = vec3(0.8);

    float diffFactor = max(dot(normal, lightDir), 0.0);
    vec3 halfDir = normalize(lightDir + viewDir);
    float specFactor = pow(max(dot(normal, halfDir), 0.0), shininess);

    vec3 ambient = ambientFac * lightColor;
    vec3 diffuse = diffFactor * lightColor;
    vec3 specular = specFactor * lightColor;

    return (ambient + diffuse + specular) * diffuseColor;

}
/**
 * Map instanceID to the flattened coordinate (0, 0) to (2, 2) for the particular face
*/
int getTopPos() {
    return 8 - instanceId % 9;
}
int getBtmPos () {
    switch(instanceId) {
        case 2: return 0;
        case 1: return 1;
        case 0: return 2;
        case 5: return 3;
        case 4: return 4;
        case 3: return 5;
        case 8: return 6;
        case 7: return 7;
        case 6: return 8;
        default: return 100;
    }
}
int getFrontPos() {
    switch(instanceId) {
        case 20: return 0;
        case 19: return 1;
        case 18: return 2;
        case 11: return 3;
        case 10: return 4;
        case 9: return 5;
        case 2: return 6;
        case 1: return 7;
        case 0: return 8;
        default: return 100;
    }
}
int getBackPos() {
    switch(instanceId) {
        case 24: return 0;
        case 25: return 1;
        case 26: return 2;
        case 15: return 3;
        case 16: return 4;
        case 17: return 5;
        case 6: return 6;
        case 7: return 7;
        case 8: return 8;
        default: return 100;
    }
}
int getLeftPos() {
    switch(instanceId) {
        case 26: return 0;
        case 23: return 1;
        case 20: return 2;
        case 17: return 3;
        case 14: return 4;
        case 11: return 5;
        case 8: return 6;
        case 5: return 7;
        case 2: return 8;
        default: return 100;
    }
}
int getRightPos() {
    switch(instanceId) {
        case 18: return 0;
        case 21: return 1;
        case 24: return 2;
        case 9: return 3;
        case 12: return 4;
        case 15: return 5;
        case 0: return 6;
        case 3: return 7;
        case 6: return 8;
        default: return 100;
    }
}

vec3 getColor(vec3 diffColor) {
    if (fragCoords.x <= 0.05 || fragCoords.x >= 0.95 ||
        fragCoords.y <= 0.05 || fragCoords.y >= 0.95)
        return vec3(0.0);
    return diffColor;
}