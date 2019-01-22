// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %s -module-name dynamicself -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %t/dynamicself.swiftmodule -typecheck -emit-objc-header-path %t/dynamicself.h -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck %s < %t/dynamicself.h
// RUN: %check-in-clang %t/dynamicself.h

// REQUIRES: objc_interop

import Foundation

// CHECK-LABEL: @protocol FooProtocol
// CHECK-NEXT:    - (nonnull instancetype)fooFunc SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT:    - (nullable instancetype)optionalFooFunc SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT:  @end

@objc protocol FooProtocol {
  func fooFunc() -> Self
  func optionalFooFunc() -> Self?
}

// CHECK-LABEL: @interface BarClass : NSObject <FooProtocol>
// CHECK-NEXT:    - (nonnull instancetype)fooFunc SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT:    - (nullable instancetype)optionalFooFunc SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT:    - (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT:  @end

@objc @objcMembers class BarClass: NSObject, FooProtocol {
  func fooFunc() -> Self { return self }
  func optionalFooFunc() -> Self? { return self }
}
