#ifndef TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_REFERENCE_COUNTED_H
#define TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_REFERENCE_COUNTED_H

#include <stdlib.h>

#ifdef __cplusplus
#include <new>
#endif

#include "visibility.h"

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

static int finalLocalRefCount = 100;

#ifdef __cplusplus

namespace NS {

struct __attribute__((swift_attr("import_as_ref")))
__attribute__((swift_attr("retain:LCRetain")))
__attribute__((swift_attr("release:LCRelease"))) LocalCount final {
  int value = 0;

  static LocalCount *create() {
    return new (malloc(sizeof(LocalCount))) LocalCount();
  }

  int returns42() { return 42; }
  int constMethod() const { return 42; }
};

}

inline void LCRetain(NS::LocalCount *x) {
  x->value++;
  finalLocalRefCount = x->value;
}
inline void LCRelease(NS::LocalCount *x) {
  x->value--;
  finalLocalRefCount = x->value;
}

static int globalCount = 0;

struct __attribute__((swift_attr("import_as_ref")))
__attribute__((swift_attr("retain:GCRetain")))
__attribute__((swift_attr("release:GCRelease"))) GlobalCount {
  static GlobalCount *create() {
    return new (malloc(sizeof(GlobalCount))) GlobalCount();
  }
};

inline void GCRetain(GlobalCount *x) { globalCount++; }
inline void GCRelease(GlobalCount *x) { globalCount--; }

struct __attribute__((swift_attr("import_as_ref")))
__attribute__((swift_attr("retain:GCRetainNullableInit")))
__attribute__((swift_attr("release:GCReleaseNullableInit")))
GlobalCountNullableInit {
  static GlobalCountNullableInit *_Nullable create(bool wantNullptr) {
    if (wantNullptr)
      return nullptr;
    return new (malloc(sizeof(GlobalCountNullableInit)))
        GlobalCountNullableInit();
  }
};

inline void GCRetainNullableInit(GlobalCountNullableInit *x) { globalCount++; }
inline void GCReleaseNullableInit(GlobalCountNullableInit *x) { globalCount--; }
#endif

typedef struct __attribute__((swift_attr("import_as_ref")))
__attribute__((swift_attr("retain:INRetain")))
__attribute__((swift_attr("release:INRelease"))) IncompleteImpl *Incomplete;

typedef struct OpaqueRefImpl *OpaqueRef;

#ifdef __cplusplus
extern "C" {
#endif

Incomplete Incomplete_create(double weight) __attribute__((swift_attr("returns_retained"))) __attribute__((swift_name("IncompleteImpl.init(weight:)")));
void INRetain(Incomplete i);
void INRelease(Incomplete i);
double Incomplete_getWeight(Incomplete i) __attribute__((swift_name("getter:IncompleteImpl.weight(self:)")));

OpaqueRef Opaque_create(void);
void OPRetain(OpaqueRef i);
void OPRelease(OpaqueRef i);

#ifdef __cplusplus
}
#endif

SWIFT_END_NULLABILITY_ANNOTATIONS

#endif // TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_REFERENCE_COUNTED_H
