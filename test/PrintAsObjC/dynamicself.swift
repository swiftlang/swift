// REQUIRES: objc_interop

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -I %S/Inputs/custom-modules -o %t %s -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %t/dynamicself.swiftmodule -typecheck -I %S/Inputs/custom-modules -emit-objc-header-path %t/dynamicself.h -import-objc-header %S/../Inputs/empty.h -disable-objc-attr-requires-foundation-module
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
