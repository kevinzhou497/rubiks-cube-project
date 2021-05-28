#version 330 core

in vec2 texCoords;
flat in int useColors;
flat in int id;
uniform sampler2D tex;
uniform sampler2D help;
uniform sampler2D charTex;
uniform sampler2D wait;
uniform bool showWait = false;
uniform bool text = false;
uniform bool showHelp = true;
uniform bool backdrop = false;
uniform bool select = false;
out vec4 fragColor;
uniform int tileColors[54];

/**
* @return the guassian blurred value of the texture at the given texCoords
*/
vec4 blur(sampler2D);
vec3 getColorFromIndex(int);
vec4 overlay(sampler2D);


void main() {
	if (text) {
		vec2 invertedTCoords = vec2(texCoords.x, 1.0 - texCoords.y);
		fragColor = texture(charTex, invertedTCoords);
		if (fragColor.a < 0.3) discard;
	}
	else if (showWait) {
		fragColor = overlay(wait);
	}
	else if (select) {
		fragColor = vec4(0.0, 0.0, 0.0, 1.0);
	}
	else if (useColors == 1) {
		vec3 overlayColor = getColorFromIndex(id);
		fragColor = vec4(overlayColor, 1.0);
	}
	else if (showHelp) {
		fragColor = overlay(help);
	} else {
		fragColor = texture(tex, texCoords); // + blur() * 0.5;
		if (backdrop) {
			fragColor.rgb *= vec3(0.4);
			fragColor.a = 0.5;
		}
	}
}
// 5x5 guassian kernel
const float[25] kernel = float[25](
	1, 4, 7, 4, 1,
	4, 16, 24, 16, 4,
	7, 26, 41, 26, 7,
	4, 16, 24, 16, 4,
	1, 4, 7, 4, 1
);
vec4 blur(sampler2D tex) {
	vec2 texOffset = 1.0 / textureSize(tex, 0);
	vec4 result = vec4(0);
    int count = 0;
	for(int i = -2; i <= 2; ++i) {
		for(int j = -2; j <= 2; ++j) {
			result += kernel[count++] / 273.0 * 
				texture(tex, vec2(
                    texCoords.x + texOffset.x * i, 
                    texCoords.y + texOffset.y * j)
                );
		}
	}
	return result;
}

vec3 getColorFromIndex(int idx) {
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

	return faceColors[tileColors[idx]];
}

vec4 overlay(sampler2D t) {
	vec2 invertedTCoords = vec2(texCoords.x, 1.0 - texCoords.y);
	vec4 overlayColor = texture(t, invertedTCoords);
	return blur(tex) * (1 - overlayColor.a) + overlayColor;
}