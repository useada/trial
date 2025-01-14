out vec4 color;
in vec2 tex_coord;

uniform sampler2D position_map;
uniform sampler2D normal_map;
uniform sampler2D noise_map;

uniform mat4 projection_matrix;
uniform mat4 view_matrix;
uniform vec2 viewport_size;
uniform vec3 samples[64];
uniform int kernel_size = 64;
uniform float radius = 8;
uniform float bias = 5;

void main(){
  vec2 noiseScale = viewport_size / 4.0;
  // get input for SSAO algorithm
  vec3 fragPos = texture(position_map, tex_coord).xyz;
  vec3 normal = texture(normal_map, tex_coord).rgb;
  vec3 randomVec = normalize(texture(noise_map, tex_coord * noiseScale).xyz);
  // bring into view space
  fragPos = vec3(view_matrix * vec4(fragPos, 1));
  normal = normalize(transpose(inverse(mat3(view_matrix))) * normal);
  // create TBN change-of-basis matrix: from tangent-space to view-space
  vec3 tangent = normalize(randomVec - normal * dot(randomVec, normal));
  vec3 bitangent = cross(normal, tangent);
  mat3 TBN = mat3(tangent, bitangent, normal);
  // iterate over the sample kernel and calculate occlusion factor
  float occlusion = 0.0;
  for(int i = 0; i < kernel_size; ++i){
    // get sample position
    vec3 ssample = TBN * samples[i]; // from tangent to view-space
    ssample = fragPos + ssample * radius; 
    
    // project sample position (to sample texture) (to get position on screen/texture)
    vec4 offset = vec4(ssample, 1.0);
    offset = projection_matrix * offset; // from view to clip-space
    offset.xyz /= offset.w; // perspective divide
    offset.xy = offset.xy * 0.5 + 0.5; // transform to range 0.0 - 1.0
    
    // get sample depth
    vec3 samplePos = texture(position_map, offset.xy).xyz;
    samplePos = vec3(view_matrix * vec4(samplePos, 1));
    float sampleDepth = samplePos.z;
    
    // range check & accumulate
    float rangeCheck = smoothstep(0.0, 1.0, radius / abs(fragPos.z - sampleDepth));
    occlusion += (sampleDepth >= ssample.z + bias ? 1.0 : 0.0) * rangeCheck;           
  }
  occlusion = 1.0 - (occlusion / kernel_size);
  
  color = vec4(pow(occlusion, 2));
}
