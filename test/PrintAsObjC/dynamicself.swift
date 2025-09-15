// REQUIRES: objc_interop

// RUN: %empty-directory(%t)

// FIXME: BEGIN -enable-source-import hackaround
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift -disable-objc-attr-requires-foundation-module
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/CoreGraphics.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/AppKit.swift
// FIXME: END -enable-source-import hackaround


// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -I %S/Inputs/custom-modules -o %t %s -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -parse-as-library %t/dynamicself.swiftmodule -typecheck -verify -emit-objc-header-path %t/dynamicself.h -I %S/Inputs/custom-modules -import-objc-header %S/../Inputs/empty.h -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck %s < %t/dynamicself.h
// RUN: %check-in-clang -I %S/Inputs/custom-modules/ %t/dynamicself.h

import Foundation

// CHECK-LABEL: @protocol FooProtocol
@objc protocol FooProtocol {
  // CHECK: - (nonnull instancetype)fooFunc SWIFT_WARN_UNUSED_RESULT;
  func fooFunc() -> Self
  // CHECK: - (nullable instancetype)optionalFooFunc SWIFT_WARN_UNUSED_RESULT;
  func optionalFooFunc() -> Self?
}
// CHECK: @end

// CHECK-LABEL: @interface BarClass : NSObject <FooProtocol>
@objc @objcMembers class BarClass: NSObject, FooProtocol {
  // CHECK: - (nonnull instancetype)fooFunc SWIFT_WARN_UNUSED_RESULT;
  func fooFunc() -> Self { return self }
  // CHECK: - (nullable instancetype)optionalFooFunc SWIFT_WARN_UNUSED_RESULT;
  func optionalFooFunc() -> Self? { return self }
  // CHECK: - (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
}
// CHECK: @end
