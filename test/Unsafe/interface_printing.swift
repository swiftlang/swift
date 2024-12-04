// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -swift-version 5 -enable-library-evolution -module-name unsafe -emit-module -o %t/unsafe.swiftmodule -emit-module-interface-path - %s -enable-experimental-feature AllowUnsafeAttribute | %FileCheck %s

// REQUIRES: swift_feature_AllowUnsafeAttribute

// CHECK: #if compiler(>=5.3) && $AllowUnsafeAttribute
// CHECK: @unsafe public func testFunction()
// CHECK: #else
// CHECK: public func testFunction()
// CHECK: #endif
@unsafe public func testFunction() { }
