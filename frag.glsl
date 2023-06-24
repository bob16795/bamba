#version 330 core

in vec2 texCoords;
in vec4 tintColor;

out vec4 color;

uniform sampler2D tex;

void main() {
  color = texture(tex, texCoords);
}