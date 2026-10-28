// RUN: %clang -target i686-unknown-windows-msvc -I %swift_src_root/stdlib/public/SwiftShims -S -emit-llvm -x c %s -o - | %FileCheck %s

// REQUIRES: CODEGENERATOR=X86

#include "swift/shims/_SwiftCOMShims.h"

#if defined(_WIN32)
typedef __typeof__(QueryInterface(0, 0, 0)) _QIResult;
typedef __typeof__(AggregatedQueryInterface(0, 0, 0)) _AggregatedQIResult;
_Static_assert(__builtin_types_compatible_p(_QIResult, long),
               "QueryInterface must return HRESULT's underlying C type");
_Static_assert(__builtin_types_compatible_p(_AggregatedQIResult, long),
               "AggregatedQueryInterface must return HRESULT's underlying C type");
#endif

__swift_int32_t callQueryInterface(void *pUnk, const void *riid,
                                   void **ppvObject) {
  return QueryInterface(pUnk, riid, ppvObject) +
         AggregatedQueryInterface(pUnk, riid, ppvObject);
}

__swift_uint32_t callLifetime(void *pUnk) {
  return AddRef(pUnk) + Release(pUnk) +
         AggregatedAddRef(pUnk) + AggregatedRelease(pUnk);
}

__swift_int32_t callQueryInterfaceFunction(
    _SwiftCOMQueryInterfaceFunction function, void *pUnk, const void *riid,
    void **ppvObject) {
  return function(pUnk, riid, ppvObject);
}

__swift_uint32_t callLifetimeFunction(_SwiftCOMLifetimeFunction function,
                                      void *pUnk) {
  return function(pUnk);
}

// CHECK-LABEL: define{{.*}} i32 @callQueryInterface
// CHECK: call x86_stdcallcc i32 {{.*}}QueryInterface
// CHECK: call x86_stdcallcc i32 {{.*}}AggregatedQueryInterface

// CHECK-LABEL: define{{.*}} i32 @callLifetime
// CHECK: call x86_stdcallcc i32 {{.*}}AddRef
// CHECK: call x86_stdcallcc i32 {{.*}}Release
// CHECK: call x86_stdcallcc i32 {{.*}}AggregatedAddRef
// CHECK: call x86_stdcallcc i32 {{.*}}AggregatedRelease

// CHECK-LABEL: define{{.*}} i32 @callQueryInterfaceFunction
// CHECK: call x86_stdcallcc i32 %{{.*}}

// CHECK-LABEL: define{{.*}} i32 @callLifetimeFunction
// CHECK: call x86_stdcallcc i32 %{{.*}}
