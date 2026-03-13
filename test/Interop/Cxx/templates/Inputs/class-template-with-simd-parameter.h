#include <simd/simd.h>

template <typename T>
struct Templated {
  T t;
};

typedef Templated<simd::uint1> TemplatedSIMDUInt1;
typedef Templated<simd::uint16> TemplatedSIMDUInt16;
typedef Templated<simd::float3> TemplatedSIMDFloat3;
typedef Templated<simd::float4> TemplatedSIMDFloat4;
typedef Templated<simd::double8> TemplatedSIMDDouble8;
