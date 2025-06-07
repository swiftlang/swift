// Please keep this file in alphabetical order!

// REQUIRES: objc_interop

// RUN: %empty-directory(%t)

// FIXME: BEGIN -enable-source-import hackaround
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift -disable-objc-attr-requires-foundation-module
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/CoreGraphics.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift
// FIXME: END -enable-source-import hackaround

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -import-objc-header %S/Inputs/swift_name.h -emit-module -o %t %s
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -import-objc-header %S/Inputs/swift_name.h -parse-as-library %t/swift_name.swiftmodule -typecheck -verify -emit-objc-header-path %t/swift_name.h

// RUN: %FileCheck %s < %t/swift_name.h
// RUN: %check-in-clang %t/swift_name.h

import Foundation

// CHECK-LABEL: @interface Test : NSObject
// CHECK-NEXT: - (NSArray<ABCStringAlias> * _Nonnull)makeArray:(ABCStringAlias _Nonnull)_ SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (void)usePoint:(struct ABCPoint)_;
// CHECK-NEXT: - (void)useAlignment:(enum ABCAlignment)_;
// CHECK-NEXT: - (NSArray<id <ABCProto>> * _Nonnull)useObjects:(ABCClass * _Nonnull)_ SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: @end
class Test : NSObject {
  @objc func makeArray(_: ZZStringAlias) -> [ZZStringAlias] { return [] }
  @objc func usePoint(_: ZZPoint) {}
  @objc func useAlignment(_: ZZAlignment) {}
  @objc func useObjects(_: ZZClass) -> [ZZProto] { return [] }
}

@objc
public enum TestE : Int{
	@objc(A2)
	case A1
	case B1
}
// CHECK: typedef SWIFT_ENUM(NSInteger, TestE, closed)
// CHECK-NEXT: {{^}} A2 SWIFT_COMPILE_NAME("A1") = 0,
// CHECK-NEXT: {{^}} TestEB1 = 1,
