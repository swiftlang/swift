// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %s -module-name requires_super -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %t/requires_super.swiftmodule -typecheck -emit-objc-header-path %t/requires_super.h -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck %s < %t/requires_super.h
// RUN: %check-in-clang %t/requires_super.h

// REQUIRES: objc_interop

import Foundation

@objc @objcMembers class Test: NSObject {
  
  // CHECK: - (void)testMethod NS_REQUIRES_SUPER;
  @requiresSuper
  func testMethod() {}

  // CHECK: - (nonnull instancetype)initWithI:(NSInteger)i OBJC_DESIGNATED_INITIALIZER NS_REQUIRES_SUPER;
  @requiresSuper
  init(i: Int) {}
}