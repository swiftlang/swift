// Please keep this file in alphabetical order!

// FIXME: Merge this into classes_objc_generics.swift once SILGen can
// handle throwing @objc methods.

// RUN: rm -rf %t
// RUN: mkdir %t

// FIXME: BEGIN -enable-source-import hackaround
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/objc-generics-sdk -I %t) -emit-module -o %t  %S/../Inputs/objc-generics-sdk/swift-modules/ObjectiveC.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/objc-generics-sdk -I %t) -emit-module -o %t  %S/../Inputs/objc-generics-sdk/swift-modules/CoreGraphics.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/objc-generics-sdk -I %t) -emit-module -o %t  %S/../Inputs/objc-generics-sdk/swift-modules/Foundation.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/objc-generics-sdk -I %t) -emit-module -o %t  %S/../Inputs/objc-generics-sdk/swift-modules/AppKit.swift
// FIXME: END -enable-source-import hackaround

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/objc-generics-sdk -I %t) %s -parse -emit-objc-header-path %t/error_handling.h -import-objc-header %S/../Inputs/empty.h
// RUN: FileCheck %s < %t/error_handling.h

import Foundation

// CHECK-LABEL: @interface Throwing1
// CHECK-NEXT: - (BOOL)method1WithError:(NSError * __nullable * __null_unspecified)error;
// CHECK-NEXT: - (Throwing1 * __nullable)method2WithError:(NSError * __nullable * __null_unspecified)error;
// CHECK-NEXT: - (NS_ARRAY(NSString *) __nullable)method3:(NSInteger)x error:(NSError * __nullable * __null_unspecified)error;
// CHECK-NEXT: - (SWIFT_NULLABILITY(nullable) instancetype)method4WithError:(NSError * __nullable * __null_unspecified)error;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class Throwing1 {
  func method1() throws { }
  func method2() throws -> Throwing1 { return self }
  func method3(x: Int) throws -> [String] { return [] }
  func method4() throws -> Self { return self }
}
