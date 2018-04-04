// REQUIRES: objc_interop

// RUN: %empty-directory(%t)

// FIXME: BEGIN -enable-source-import hackaround
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/CoreGraphics.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/AppKit.swift
// FIXME: END -enable-source-import hackaround

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -I %S/Inputs/custom-modules -o %t %s -disable-objc-attr-requires-foundation-module -swift-version 3
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -parse-as-library %t/versioned.swiftmodule -typecheck -I %S/Inputs/custom-modules -emit-objc-header-path %t/versioned.h -import-objc-header %S/../Inputs/empty.h -disable-objc-attr-requires-foundation-module -swift-version 3
// RUN: %FileCheck %s < %t/versioned.h
// RUN: %check-in-clang -I %S/Inputs/custom-modules/ %t/versioned.h
// RUN: %check-in-clang -I %S/Inputs/custom-modules/ -fno-modules -Qunused-arguments %t/versioned.h -include Foundation.h -include VersionedFMWK.h

import VersionedFMWK

// CHECK-LABEL: @interface NullabilitySub
@objc class NullabilitySub: NullabilityBase {
  // CHECK-NEXT: - (void)processNowNullableArgument:(NSObject * _Nonnull)object;
  override func processNowNullableArgument(_ object: NSObject) {}
  // CHECK-NEXT: - (nullable instancetype)initFormerlyFailableValue:(NSInteger)value OBJC_DESIGNATED_INITIALIZER;
} // CHECK-NEXT: @end

// CHECK-LABEL: @interface UsesNestedClass
@objc class UsesNestedClass : NSObject {
  // CHECK-NEXT: - (InnerClass * _Nullable)foo SWIFT_WARN_UNUSED_RESULT;
  @objc func foo() -> InnerClass? { return nil }
  // CHECK-NEXT: - (void)fooStruct:(struct InnerStruct)_;
  @objc func fooStruct(_: InnerStruct) {}
  // CHECK-NEXT: - (void)fooAnonStruct:(InnerAnonStruct)_;
  @objc func fooAnonStruct(_: InnerAnonStruct) {}
  // CHECK-NEXT: - (void)fooAlias:(InnerAlias)_;
  @objc func fooAlias(_: InnerAlias) {}

  // CHECK-NEXT: - (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
}
// CHECK-NEXT: @end
