// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/custom-modules -emit-silgen -o - %s | FileCheck %s

// REQUIRES: objc_interop

import ObjectiveC
import Foundation
import objc_ext
import TestProtocols

class A {
  @objc func foo() -> String? {
    return ""
  }
// CHECK-LABEL:    sil hidden @_TToFC8optional1A3foofS0_FT_GSqSS_ : $@convention(objc_method) (A) -> @autoreleased Optional<NSString>
// CHECK:      [[T0:%.*]] = function_ref @_TFC8optional1A3foofS0_FT_GSqSS_
// CHECK-NEXT: [[T1:%.*]] = apply [[T0]](%0)
// CHECK-NEXT: strong_release
// CHECK:      [[T2:%.*]] = select_enum [[T1]]
// CHECK-NEXT: cond_br [[T2]]
//   Something branch: project value, translate, inject into result.
// CHECK:      [[STR:%.*]] = unchecked_enum_data [[T1]]
// CHECK:      [[T0:%.*]] = function_ref @swift_StringToNSString
// CHECK-NEXT: [[T1:%.*]] = apply [[T0]]([[STR]])
// CHECK-NEXT: enum $Optional<NSString>, #Optional.Some!enumelt.1, [[T1]]
// CHECK-NEXT: br
//   Nothing branch: inject nothing into result.
// CHECK:      enum $Optional<NSString>, #Optional.None!enumelt
// CHECK-NEXT: br
//   Continuation.
// CHECK:      bb3([[T0:%.*]] : $Optional<NSString>):
// CHECK-NEXT: autorelease_return [[T0]]

  @objc func bar(#x : String?) {}
// CHECK-LABEL:    sil hidden @_TToFC8optional1A3barfS0_FT1xGSqSS__T_ : $@convention(objc_method) (Optional<NSString>, A) -> ()
// CHECK:      [[T1:%.*]] = select_enum %0
// CHECK-NEXT: cond_br [[T1]]
//   Something branch: project value, translate, inject into result.
// CHECK:      [[NSSTR:%.*]] = unchecked_enum_data %0
// CHECK:      [[T0:%.*]] = function_ref @swift_NSStringToString
//   Make a temporary initialized string that we're going to clobber as part of the conversion process (?).
// CHECK-NEXT: [[NSSTR_BOX:%.*]] = enum $Optional<NSString>, #Optional.Some!enumelt.1, [[NSSTR]] : $NSString
// CHECK-NEXT: [[T1:%.*]] = apply [[T0]]([[NSSTR_BOX]])
// CHECK-NEXT: enum $Optional<String>, #Optional.Some!enumelt.1, [[T1]]
// CHECK-NEXT: br
//   Nothing branch: inject nothing into result.
// CHECK:      enum $Optional<String>, #Optional.None!enumelt
// CHECK-NEXT: br
//   Continuation.
// CHECK:      bb3([[T0:%.*]] : $Optional<String>):
// CHECK:      [[T1:%.*]] = function_ref @_TFC8optional1A3barfS0_FT1xGSqSS__T_
// CHECK-NEXT: [[T2:%.*]] = apply [[T1]]([[T0]], %1)
// CHECK-NEXT: strong_release %1
// CHECK-NEXT: return [[T2]] : $()
}


// rdar://15144951
class TestWeak : NSObject {
  weak var b : WeakObject? = nil
}
class WeakObject : NSObject {}
