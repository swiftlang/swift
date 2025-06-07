// Test that known types that conform to `_ObjectiveCBridgeable` import or
// forward-declare based on the Clang type in their known type mapping, not
// their bridged type.
//
// This is particularly important for `CGFloat`, which has a native Swift decl
// in the CoreFoundation overlay that shadows the imported Clang decl, so
// relying solely on whether or not the decl has a Clang node is not sufficient.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -parse-as-library -typecheck -verify -emit-objc-header-path %t/swift.h
// RUN: %FileCheck %s < %t/swift.h
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-module -o %t %clang-importer-sdk-path/swift-modules/CoreFoundation.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource) -emit-module -o %t %clang-importer-sdk-path/swift-modules/CoreGraphics.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-module -o %t %clang-importer-sdk-path/swift-modules/Foundation.swift

// REQUIRES: objc_interop

import CoreGraphics
import Foundation

// CHECK-NOT: @class NSNumber;

// CHECK-LABEL: @interface Test : NSObject{{$}}
public class Test: NSObject {
  // CHECK-NEXT: - (CGFloat)level
  @objc public func level() -> CGFloat { 9000.0 }
  // CHECK-NEXT: - (BOOL)isEnabled
  @objc public func isEnabled() -> Bool { true }
  // CHECK-NEXT: init
} // CHECK-NEXT: @end
