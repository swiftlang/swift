// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library -emit-module %S/Inputs/custom-modules/CircularLibrary.swift -I %S/Inputs/custom-modules -o %t/CircularLibrary.swiftmodule
// RUN: not --crash %target-swift-frontend -emit-sil -O %s -I %S/Inputs/custom-modules -I %t -o %t/pretty_printed_decls.sil 2>%t.txt
// RUN: %FileCheck %s <%t.txt

// FIXME: This test case should probably succeed, not fail; a lookup of
// MyWholeNumber.RawValue during the GenericSpecializer pass causes a recursive
// lookup of MyWholeNumber.RawValue while trying to resolve the
// MyWholeNumber.Stride typealias, creating a request cycle. (SR-14324)
//
// For now, we'll use this test to make sure that we get a useful diagnostic
// when this happens, but in the long run we should actually fix this bug.

// REQUIRES: objc_interop

// The "not --crash" is only true for an asserts compiler.
// REQUIRES: asserts

import CircularLibrary

func fn() {
  var array: [MyNumberwang] = []
  array.sort { $0.number < $1.number }
}
fn()

//      CHECK: {{^}}CircularLibrary.MyWholeNumber:1:{{[0-9]+}}: error: circular reference
// CHECK-NEXT: {{^}}public enum MyWholeNumber : UInt32 {
// CHECK-NEXT: {{^}}            ^
