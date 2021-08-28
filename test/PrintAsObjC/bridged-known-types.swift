// Test that known types that conform to `_ObjectiveCBridgeable` import or
// forward-declare based on the Clang type in their known type mapping, not
// their bridged type.
//
// This is particularly important for `CGFloat`, which has a native Swift decl
// in the CoreGraphics overlay that shadows the imported Clang decl, so relying
// solely on whether or not the decl has a Clang node is not sufficient.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck %s -parse-as-library -emit-objc-header-path %t/swift.h
// RUN: %FileCheck %s < %t/swift.h

// REQUIRES: objc_interop

import CoreGraphics
import Foundation

// CHECK: @import CoreGraphics;

// CHECK-NOT: @class NSNumber;

// CHECK-LABEL: @interface Test : NSObject{{$}}
public class Test: NSObject {
  // CHECK-NEXT: - (CGFloat)level
  @objc public func level() -> CGFloat { 9000.0 }
  // CHECK-NEXT: - (BOOL)isEnabled
  @objc public func isEnabled() -> Bool { true }
  // CHECK-NEXT: init
} // CHECK-NEXT: @end
