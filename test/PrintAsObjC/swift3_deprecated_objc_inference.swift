// REQUIRES: objc_interop

// RUN: %empty-directory(%t)

// FIXME: BEGIN -enable-source-import hackaround
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/CoreGraphics.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/AppKit.swift
// FIXME: END -enable-source-import hackaround


// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -I %S/Inputs/custom-modules -o %t %s -disable-objc-attr-requires-foundation-module -swift-version 4 -enable-swift3-objc-inference -warn-swift3-objc-inference-minimal
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -parse-as-library %t/swift3_deprecated_objc_inference.swiftmodule -typecheck -I %S/Inputs/custom-modules -emit-objc-header-path %t/swift3_deprecated_objc_inference.h -import-objc-header %S/../Inputs/empty.h -disable-objc-attr-requires-foundation-module -swift-version 4 -enable-swift3-objc-inference -warn-swift3-objc-inference-minimal
// RUN: %FileCheck %s < %t/swift3_deprecated_objc_inference.h
// RUN: %check-in-clang -I %S/Inputs/custom-modules/ %t/swift3_deprecated_objc_inference.h

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -I %S/Inputs/custom-modules -o %t/swift3_deprecated_objc_inference_nowarn.swiftmodule %s -disable-objc-attr-requires-foundation-module -swift-version 4 -enable-swift3-objc-inference
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -parse-as-library %t/swift3_deprecated_objc_inference_nowarn.swiftmodule -typecheck -I %S/Inputs/custom-modules -emit-objc-header-path %t/swift3_deprecated_objc_inference_nowarn.h -import-objc-header %S/../Inputs/empty.h -disable-objc-attr-requires-foundation-module -swift-version 4 -enable-swift3-objc-inference
// RUN: %FileCheck -check-prefix=CHECK-NOWARN %s < %t/swift3_deprecated_objc_inference_nowarn.h
// RUN: %check-in-clang -I %S/Inputs/custom-modules/ %t/swift3_deprecated_objc_inference_nowarn.h

import Foundation

// CHECK-NOWARN-NOT: @objc

// CHECK-LABEL: @interface A1{{$}}
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class A1 {
}

// CHECK-LABEL: @interface A2{{$}}
// CHECK-NEXT: - (nonnull instancetype)initWithA2:(A2 * _Nonnull)a2 OBJC_DESIGNATED_INITIALIZER SWIFT_DEPRECATED_OBJC("Swift initializer 'A2.init(a2:)' uses '@objc' inference deprecated in Swift 4; add '@objc' to provide an Objective-C entrypoint");
// CHECK-NEXT: - (void)foo SWIFT_DEPRECATED_OBJC("Swift method 'A2.foo()' uses '@objc' inference deprecated in Swift 4; add '@objc' to provide an Objective-C entrypoint");
// CHECK-NEXT: + (void)bar SWIFT_DEPRECATED_OBJC("Swift method 'A2.bar()' uses '@objc' inference deprecated in Swift 4; add '@objc' to provide an Objective-C entrypoint");
// CHECK-NEXT: @property (nonatomic, strong) A2 * _Nullable property SWIFT_DEPRECATED_OBJC("Swift property 'A2.property' uses '@objc' inference deprecated in Swift 4; add '@objc' to provide an Objective-C entrypoint");
// CHECK-NEXT: SWIFT_CLASS_PROPERTY(@property (nonatomic, class, strong) A2 * _Nullable static_property SWIFT_DEPRECATED_OBJC("Swift property 'A2.static_property' uses '@objc' inference deprecated in Swift 4; add '@objc' to provide an Objective-C entrypoint");)
// CHECK-NEXT: + (A2 * _Nullable)static_property SWIFT_WARN_UNUSED_RESULT SWIFT_DEPRECATED_OBJC("Swift property 'A2.static_property' uses '@objc' inference deprecated in Swift 4; add '@objc' to provide an Objective-C entrypoint");
// CHECK-NEXT: + (void)setStatic_property:(A2 * _Nullable)value SWIFT_DEPRECATED_OBJC("Swift property 'A2.static_property' uses '@objc' inference deprecated in Swift 4; add '@objc' to provide an Objective-C entrypoint");
// CHECK-NEXT: - (A2 * _Nonnull)objectAtIndexedSubscript:(NSInteger)i SWIFT_WARN_UNUSED_RESULT SWIFT_DEPRECATED_OBJC("Swift subscript 'A2.subscript(_:)' uses '@objc' inference deprecated in Swift 4; add '@objc' to provide an Objective-C entrypoint");
// CHECK-NEXT: - (void)setObject:(A2 * _Nonnull)newValue atIndexedSubscript:(NSInteger)i SWIFT_DEPRECATED_OBJC("Swift subscript 'A2.subscript(_:)' uses '@objc' inference deprecated in Swift 4; add '@objc' to provide an Objective-C entrypoint");

// CHECK-NEXT: @end
@objc class A2 {
  init(a2: A2) { }
  func foo() { }
  class func bar() { }
  var property: A2? = nil
  static var static_property: A2? = nil
  subscript (i: Int) -> A2 { get { return self } set { } }
}
