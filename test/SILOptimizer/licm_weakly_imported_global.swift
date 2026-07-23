// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -O -emit-sil -I %t/GatedGlobals %t/main.swift | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

// LICM must not speculatively hoist loads from weakly-imported globals out of
// #available-guarded blocks: the address of a weakly-imported global is null at
// runtime if the symbol is unavailable (its availability lies in the future
// relative to the deployment target), so the hoisted load would dereference
// null and crash - even though the source only reads the global behind a
// matching `#available` check, which the compiler itself requires.
//
// This happened in practice with AVFoundation's availability-gated constants,
// e.g. `AVCaptureDeviceTypeMicrophone` (iOS 17+): comparing against it inside
// a loop behind `#available(iOS 17.0, *)` crashed with EXC_BAD_ACCESS at 0x0
// on iOS 16 in -O builds, because the load of the constant was hoisted into
// the loop preheader.
// See https://github.com/swiftlang/swift/issues/90916

//--- GatedGlobals/module.modulemap
module GatedGlobals {
  header "gated.h"
  export *
}

//--- GatedGlobals/gated.h
#import <Foundation/Foundation.h>
extern NSString * const gatedName __attribute__((availability(macos,introduced=99)));
extern NSString * const ungatedName;

//--- main.swift
import GatedGlobals

// The load of `gatedName` must stay behind the availability check and must not
// be moved into the loop preheader. The load of `ungatedName` (strongly linked)
// may still be hoisted.

// CHECK-LABEL: sil {{.*}}@${{.*}}countGatedMatches
// CHECK: [[GATED:%[0-9]+]] = global_addr @gatedName
// CHECK-NOT: load [[GATED]]
// CHECK: apply {{.*}}isOSVersionAtLeast
// CHECK-NOT: load [[GATED]]
// CHECK: cond_br
// CHECK: load [[GATED]]
// CHECK: } // end sil function '${{.*}}countGatedMatches
@inline(never)
public func countGatedMatches(_ names: [String]) -> Int {
  var count = 0
  for name in names {
    if #available(macOS 99, *) {
      if name == gatedName {
        count += 1
      }
    } else {
      if name == ungatedName {
        count += 1
      }
    }
  }
  return count
}
