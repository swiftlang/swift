typedef int int3 __attribute__((__ext_vector_type__(3)));
typedef float float3 __attribute__((__ext_vector_type__(3)));
typedef float float4 __attribute__((__ext_vector_type__(4)));
typedef double double2 __attribute__((__ext_vector_type__(2)));
typedef unsigned char byte17 __attribute__((__ext_vector_type__(17)));

int3    makes_int3(void);
float3  makes_float3(void);
float4  makes_float4(void);
double2 makes_double2(void);
byte17  makes_byte17(void);

void takes_int3(int3 x);
void takes_float3(float3 x);
void takes_float4(float4 x);
void takes_double2(double2 x);
void takes_byte17(byte17 x);
