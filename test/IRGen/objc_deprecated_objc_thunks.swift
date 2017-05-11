// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil %s -emit-ir -disable-objc-attr-requires-foundation-module -enable-swift3-objc-inference -swift-version 4 | %FileCheck -check-prefix CHECK-SWIFT4 %s

// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil %s -emit-ir -disable-objc-attr-requires-foundation-module -swift-version 3 | %FileCheck -check-prefix CHECK-SWIFT3 %s

// REQUIRES: CPU=x86_64
// REQUIRES: objc_interop

import Foundation

class ObjCSubclass : NSObject {
  func foo() { }
}

// CHECK-SWIFT4-LABEL: define hidden void @_T0016objc_deprecated_A7_thunks12ObjCSubclassC3fooyyFTo(%0*, i8*)
// CHECK-SWIFT4: entry:
// CHECK-SWIFT4: [[SELF:%[0-9]+]] = bitcast %0* %0 to %objc_object*
// CHECK-SWIFT4-NEXT: call void @swift_objc_swift3ImplicitObjCEntrypoint(%objc_object* [[SELF]], i8* %1)

// CHECK-SWIFT3-LABEL: define hidden void @_T0016objc_deprecated_A7_thunks12ObjCSubclassC3fooyyFTo(%0*, i8*)
// CHECK-SWIFT3: entry:
// CHECK-SWIFT3-NOT: call void @swift_objc_swift3ImplicitObjCEntrypoint(%objc_object* [[SELF]], i8* %1)
