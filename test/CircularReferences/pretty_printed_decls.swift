// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library -emit-module %S/Inputs/custom-modules/CircularLibrary.swift -I %S/Inputs/custom-modules -o %t/CircularLibrary.swiftmodule
// RUN: not %target-swift-frontend -typecheck %s -I %S/Inputs/custom-modules -I %t -debug-cause-cycle-via-decl MyWholeNumber.RawValue -debug-cause-cycle-via-decl MyWholeNumber -debug-cause-cycle-via-decl MyNumberwang -debug-cycles 2>%t.txt
// RUN: %FileCheck %s <%t.txt

// REQUIRES: objc_interop

import CircularLibrary

// FIXME: This first failure is actually a bug! It's a purer form of the test
//        case in test/CircularReferences/overlay_extensions.swift. (SR-14324)
// CHECK-LABEL: {{^}}=== CYCLE DETECTED ===
//   CHECK-NOT: error: circular reference
//       CHECK: {{^}}==> DirectLookupRequest(directly looking up 'RawValue' on CircularLibrary.(file).MyWholeNumber with options
//  CHECK-NEXT: {{^}}CircularLibrary.MyWholeNumber:{{[0-9]+:[0-9]+}}: error: circular reference
//  CHECK-NEXT: {{^}}public enum MyWholeNumber : UInt32 {
//  CHECK-NEXT: {{^}}            ^

// This failure is the one the test case is actually intended to cause.
// CHECK-LABEL: {{^}}=== CYCLE DETECTED ===
//  CHECK-NEXT: {{^}}==> DebugIntentionallyCauseCycleRequest(Swift.(file).UInt32, {CircularLibrary.(file).MyWholeNumber, CircularLibrary.(file).MyNumberwang})
//  CHECK-NEXT: {{^}}    DebugIntentionallyCauseCycleRequest(CircularLibrary.(file).MyWholeNumber, {CircularLibrary.(file).MyNumberwang, Swift.(file).UInt32})
//  CHECK-NEXT: {{^}}    DebugIntentionallyCauseCycleRequest(CircularLibrary.(file).MyNumberwang, {Swift.(file).UInt32, CircularLibrary.(file).MyWholeNumber})
//  CHECK-NEXT: {{^}}Swift.UInt32:{{[0-9]+:[0-9]+}}: error: circular reference
//  CHECK-NEXT: {{^}}@frozen public struct UInt32 : FixedWidthInteger, UnsignedInteger, _ExpressibleByBuiltinIntegerLiteral {
//  CHECK-NEXT: {{^}}                      ^
//  CHECK-NEXT: {{^}}CircularLibrary.MyNumberwang:{{[0-9]+:[0-9]+}}: note: through reference here
//  CHECK-NEXT: {{^}}open class MyNumberwang {
//  CHECK-NEXT: {{^}}           ^
//  CHECK-NEXT: {{^}}CircularLibrary.MyWholeNumber:{{[0-9]+:[0-9]+}}: note: through reference here
//  CHECK-NEXT: {{^}}public enum MyWholeNumber : UInt32 {
//  CHECK-NEXT: {{^}}            ^
