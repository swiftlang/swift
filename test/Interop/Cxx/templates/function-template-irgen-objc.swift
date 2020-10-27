// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s

// TODO: This needs to be fixed in the SwiftTypeConverter.
// Currently "Any" is imported as an Objective-C "id".
// That doesn't work unless we have Objective-C interop.
// Once that's done, this test can be merged with "template-irgen".
// REQUIRES: objc_interop

import FunctionTemplates

// CHECK-LABEL: define {{.*}}void @"$s4main18testPassThroughAny1xypyp_tF"(%Any* noalias nocapture sret %0, %Any* noalias nocapture dereferenceable({{32|16}}) %1)
// CHECK: call i8* @_Z11passThroughIP11objc_objectET_S2_(i8*
// CHECK: ret void
public func testPassThroughAny(x: Any) -> Any {
  return passThrough(x)
}

