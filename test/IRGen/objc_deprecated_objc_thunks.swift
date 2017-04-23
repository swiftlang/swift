// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil %s -emit-ir -disable-objc-attr-requires-foundation-module | %FileCheck %s

// REQUIRES: CPU=x86_64
// REQUIRES: objc_interop

import Foundation

class ObjCSubclass : NSObject {
  func foo() { }
}

// CHECK-LABEL: define hidden void @_T0016objc_deprecated_A7_thunks12ObjCSubclassC3fooyyFTo(%0*, i8*)
// CHECK: entry:
// CHECK: [[SELF:%[0-9]+]] = bitcast %0* %0 to %objc_object*
// CHECK-NEXT: call void @swift_objc_swift3ImplicitObjCEntrypoint(%objc_object* [[SELF]], i8* %1)