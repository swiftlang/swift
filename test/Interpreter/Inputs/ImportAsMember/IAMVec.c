#include "IAMVec.h"
#include <math.h>

#ifdef __cplusplus
extern "C" {
#endif

extern double IAMVec3GetNorm(IAMVec3Ref self) {
  double x = self->x, y = self->y, z = self->z;
  return sqrt(x * x + y * y + z * z);
}

#ifdef __cplusplus
}
#endif
