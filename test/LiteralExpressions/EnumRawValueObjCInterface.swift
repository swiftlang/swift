// An '@objc' enum case's raw value is its Objective-C enumerator value, so a
// textual interface prints it. Constant folding replaces the written expression
// with an implicit literal, which is why explicitness comes from the original
// expression rather than from the folded one.
// REQUIRES: objc_interop
// REQUIRES: swift_feature_LiteralExpressions
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -module-name EnumRawValueObjCInterface -o %t/EnumRawValueObjCInterface.swiftmodule -emit-module-interface-path %t/EnumRawValueObjCInterface.swiftinterface -enable-library-evolution -swift-version 5 %s -enable-experimental-feature LiteralExpressions
// RUN: %FileCheck %s < %t/EnumRawValueObjCInterface.swiftinterface

import Foundation

// CHECK-LABEL: @objc public enum E : Swift::Int {
@objc public enum E: Int {
  // CHECK-NEXT: case literal = 1
  case literal = 1
  // CHECK-NEXT: case sum = 5
  case sum = 2 + 3
  // An auto-incremented case has no written raw value and prints without one.
  // CHECK-NEXT: case autoIncremented{{$}}
  case autoIncremented
  // CHECK-NEXT: case negative = -7
  case negative = -7
  // CHECK-NEXT: case product = 32
  case product = 0x10 * 2
}
