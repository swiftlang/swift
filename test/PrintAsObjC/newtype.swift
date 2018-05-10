// Please keep this file in alphabetical order!

// REQUIRES: objc_interop

// RUN: %empty-directory(%t)

// FIXME: BEGIN -enable-source-import hackaround
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/CoreGraphics.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift
// FIXME: END -enable-source-import hackaround

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -import-objc-header %S/Inputs/newtype.h -emit-module -o %t %s
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -import-objc-header %S/Inputs/newtype.h -parse-as-library %t/newtype.swiftmodule -typecheck -emit-objc-header-path %t/newtype.h

// RUN: %FileCheck %s < %t/newtype.h

// RUN: %check-in-clang %t/newtype.h
// RUN: %check-in-clang -fno-modules -Qunused-arguments %t/newtype.h

import Foundation

// CHECK-LABEL: @interface TestEnumLike : NSObject
class TestEnumLike : NSObject {
  // CHECK: - (void)takesNewtype:(EnumLikeStringWrapper _Nonnull)a;
  @objc func takesNewtype(_ a: EnumLikeStringWrapper) {}
  // CHECK: - (void)takesNewtypeArray:(NSArray<EnumLikeStringWrapper> * _Nonnull)a;
  @objc func takesNewtypeArray(_ a: [EnumLikeStringWrapper]) {}
  // CHECK: - (void)takesNewtypeDictionary:(NSDictionary<EnumLikeStringWrapper, EnumLikeStringWrapper> * _Nonnull)a;
  @objc func takesNewtypeDictionary(_ a: [EnumLikeStringWrapper: EnumLikeStringWrapper]) {}
  // CHECK: - (void)takesNewtypeOptional:(EnumLikeStringWrapper _Nullable)a;
  @objc func takesNewtypeOptional(_ a: EnumLikeStringWrapper?) {}
}
// CHECK: @end

// CHECK-LABEL: @interface TestStructLike : NSObject
class TestStructLike : NSObject {
  // CHECK: - (void)takesNewtype:(StructLikeStringWrapper _Nonnull)a;
  @objc func takesNewtype(_ a: StructLikeStringWrapper) {}
  // CHECK: - (void)takesNewtypeArray:(NSArray<StructLikeStringWrapper> * _Nonnull)a;
  @objc func takesNewtypeArray(_ a: [StructLikeStringWrapper]) {}
  // CHECK: - (void)takesNewtypeDictionary:(NSDictionary<StructLikeStringWrapper, StructLikeStringWrapper> * _Nonnull)a;
  @objc func takesNewtypeDictionary(_ a: [StructLikeStringWrapper: StructLikeStringWrapper]) {}
  // CHECK: - (void)takesNewtypeOptional:(StructLikeStringWrapper _Nullable)a;
  @objc func takesNewtypeOptional(_ a: StructLikeStringWrapper?) {}

  // CHECK: - (void)takesNewtypeObjectDictionary:(NSDictionary<StructLikeObjectWrapper, StructLikeObjectWrapper> * _Nonnull)a;
  @objc func takesNewtypeObjectDictionary(_ a: [StructLikeObjectWrapper: StructLikeObjectWrapper]) {}

  // CHECK: - (void)takesNewtypeErrorArray:(NSArray<StructLikeErrorWrapper> * _Nonnull)a;
  @objc func takesNewtypeErrorArray(_ a: [StructLikeErrorWrapper]) {}
}
// CHECK: @end

