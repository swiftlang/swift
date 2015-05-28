extern int globalValue;

static const int staticGlobalValue = 1171;

const int *getLocation() {
  return &staticGlobalValue;
}

typedef float __attribute__((__ext_vector_type__(4))) float4;

static const float4 staticGlobalValueSIMD = { 1.0, 2.0, 3.0, 4.0 };

const float4 *getLocationSIMD() {
  return &staticGlobalValueSIMD;
}
