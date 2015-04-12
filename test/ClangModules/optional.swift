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
// CHECK:    sil hidden @_TToFC8optional1A3foofS0_FT_GSqSS_ : $@cc(objc_method) @thin (A) -> @autoreleased Optional<NSString>
// CHECK:      [[T0:%.*]] = function_ref @_TFC8optional1A3foofS0_FT_GSqSS_
// CHECK-NEXT: [[T1:%.*]] = apply [[T0]](%0)
// CHECK-NEXT: strong_release
// CHECK-NEXT: [[TMP_OPTNSSTR:%.*]] = alloc_stack $Optional<NSString>
// CHECK-NEXT: [[TMP_OPTSTR:%.*]] = alloc_stack $Optional<String>
// CHECK-NEXT: store [[T1]] to [[TMP_OPTSTR]]#1
// CHECK:      [[T1:%.*]] = select_enum_addr [[TMP_OPTSTR]]
// CHECK-NEXT: cond_br [[T1]]
//   Something branch: project value, translate, inject into result.
// CHECK:      [[TMP_STR:%.*]] = unchecked_take_enum_data_addr [[TMP_OPTSTR]]
// CHECK-NEXT: [[STR:%.*]] = load [[TMP_STR]]
// CHECK:      [[T0:%.*]] = function_ref @swift_StringToNSString
// CHECK-NEXT: [[T1:%.*]] = apply [[T0]]([[STR]])
// CHECK-NEXT: [[TMP_NSSTR:%.*]] = init_enum_data_addr [[TMP_OPTNSSTR]]
// CHECK-NEXT: store [[T1]] to [[TMP_NSSTR]]
// CHECK-NEXT: inject_enum_addr [[TMP_OPTNSSTR]]{{.*}}Some
// CHECK-NEXT: br
//   Nothing branch: inject nothing into result.
// CHECK:      inject_enum_addr [[TMP_OPTNSSTR]]{{.*}}None
// CHECK-NEXT: br
//   Continuation.
// CHECK:      [[T0:%.*]] = load [[TMP_OPTNSSTR]]
// CHECK-NEXT: dealloc_stack [[TMP_OPTSTR]]
// CHECK-NEXT: dealloc_stack [[TMP_OPTNSSTR]]
// CHECK-NEXT: autorelease_return [[T0]]

  @objc func bar(#x : String?) {}
// CHECK:    sil hidden @_TToFC8optional1A3barfS0_FT1xGSqSS__T_ : $@cc(objc_method) @thin (Optional<NSString>, A) -> ()
// CHECK:      [[TMP_OPTSTR:%.*]] = alloc_stack $Optional<String>
// CHECK-NEXT: [[TMP_OPTNSSTR:%.*]] = alloc_stack $Optional<NSString>
// CHECK-NEXT: store {{.*}} to [[TMP_OPTNSSTR]]#1
// CHECK:      [[T1:%.*]] = select_enum_addr [[TMP_OPTNSSTR]]
// CHECK-NEXT: cond_br [[T1]]
//   Something branch: project value, translate, inject into result.
// CHECK:      [[TMP_NSSTR:%.*]] = unchecked_take_enum_data_addr [[TMP_OPTNSSTR]]
// CHECK-NEXT: [[NSSTR:%.*]] = load [[TMP_NSSTR]]
// CHECK:      [[T0:%.*]] = function_ref @swift_NSStringToString
//   Make a temporary initialized string that we're going to clobber as part of the conversion process (?).
// CHECK-NEXT: [[NSSTR_BOX:%.*]] = enum $Optional<NSString>, #Optional.Some!enumelt.1, [[NSSTR]] : $NSString
// CHECK-NEXT: [[T1:%.*]] = apply [[T0]]([[NSSTR_BOX]])
// CHECK-NEXT: [[TMP_STR2:%.*]] = init_enum_data_addr [[TMP_OPTSTR]]
// CHECK-NEXT: store [[T1]] to [[TMP_STR2]]
// CHECK-NEXT: inject_enum_addr [[TMP_OPTSTR]]{{.*}}Some
// CHECK-NEXT: br
//   Nothing branch: inject nothing into result.
// CHECK:      inject_enum_addr [[TMP_OPTSTR]]{{.*}}None
// CHECK-NEXT: br
//   Continuation.
// CHECK:      [[T0:%.*]] = load [[TMP_OPTSTR]]
// CHECK-NEXT: dealloc_stack [[TMP_OPTNSSTR]]
// CHECK-NEXT: dealloc_stack [[TMP_OPTSTR]]
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
