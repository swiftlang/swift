// RUN: %empty-directory(%t)
// RUN: split-file %s %t
//
// RUN: %target-clang -fobjc-arc %t/Obs.m -c -o %t/Obs.o
// RUN: %target-build-swift -I %t -Xlinker %t/Obs.o %t/main.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

// rdar://189356270
// Calling an optional @objc protocol requirement through optional chaining
// goes through a partial application forwarder. A C struct argument that is
// passed indirectly in the native Swift convention must still be passed using
// the C ABI to the ObjC method.

//--- module.modulemap
module Obs {
  header "Obs.h"
  export *
}

//--- Obs.h
#import <Foundation/Foundation.h>
#include <simd/simd.h>

typedef struct {
  simd_float3 scale;
  simd_quatf rotation;
  simd_float3 translation;
} SRT;

@protocol Observer <NSObject>
@optional
- (void)didUpdateTransform:(SRT)t;
- (void)didUpdateTransform:(SRT)t context:(id)ctx;
@end

@interface Impl : NSObject <Observer>
@end

//--- Obs.m
#import "Obs.h"

@implementation Impl
- (void)didUpdateTransform:(SRT)t {
  printf("2-arg scale=(%g,%g,%g) trans=(%g,%g,%g)\n",
         t.scale.x, t.scale.y, t.scale.z,
         t.translation.x, t.translation.y, t.translation.z);
}
- (void)didUpdateTransform:(SRT)t context:(id)ctx {
  printf("3-arg scale=(%g,%g,%g) trans=(%g,%g,%g) context=%s\n",
         t.scale.x, t.scale.y, t.scale.z,
         t.translation.x, t.translation.y, t.translation.z,
         [[ctx description] UTF8String]);
}
@end

//--- main.swift
import Foundation
import Obs

setvbuf(stdout, nil, _IONBF, 0)

var sent = SRT()
sent.scale = SIMD3<Float>(2, 3, 4)
sent.rotation = simd_quatf(ix: 0, iy: 0, iz: 0, r: 1)
sent.translation = SIMD3<Float>(5, 6, 7)

let impl = Impl()

// CHECK-LABEL: direct call
print("direct call")
// CHECK-NEXT: 2-arg scale=(2,3,4) trans=(5,6,7)
impl.didUpdateTransform(sent)
// CHECK-NEXT: 3-arg scale=(2,3,4) trans=(5,6,7) context=ctx
impl.didUpdateTransform(sent, context: "ctx" as NSString)

let observer: Observer = impl

// CHECK-LABEL: optional requirement call
print("optional requirement call")
// CHECK-NEXT: 2-arg scale=(2,3,4) trans=(5,6,7)
observer.didUpdateTransform?(sent)
// CHECK-NEXT: 3-arg scale=(2,3,4) trans=(5,6,7) context=ctx
observer.didUpdateTransform?(sent, context: "ctx" as NSString)

// CHECK-LABEL: done
print("done")
