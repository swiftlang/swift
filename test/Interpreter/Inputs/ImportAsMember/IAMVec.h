#ifndef IAMVEC_H
#define IAMVEC_H

#ifdef __cplusplus
extern "C" {
#endif

struct __attribute__((swift_name("Vec3"))) IAMVec3 {
  double x, y, z;
};
typedef struct IAMVec3 *IAMVec3Ref;

extern double IAMVec3GetNorm(IAMVec3Ref)
    __attribute__((swift_name("getter:Vec3.norm(self:)")));

#ifdef __cplusplus
}
#endif


#endif // IAMVEC_H
