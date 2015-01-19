// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend %clang-importer-sdk -enable-source-import -emit-module -emit-module-doc -o %t %s -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend %clang-importer-sdk -parse-as-library %t/enums.swiftmodule -parse -emit-objc-header-path %t/enums.h -import-objc-header %S/../Inputs/empty.h -disable-objc-attr-requires-foundation-module
// RUN: FileCheck %s < %t/enums.h
// RUN: %check-in-clang %t/enums.h
// RUN: %check-in-clang -fno-modules %t/enums.h -include Foundation.h -include ctypes.h -include CoreFoundation.h

import Foundation

// CHECK-LABEL: enum FooComments : NSInteger;
// CHECK-LABEL: enum NegativeValues : int16_t;
// CHECK-LABEL: @interface AnEnumMethod
// CHECK-LABEL: - (enum NegativeValues)takeAndReturnEnum:(enum FooComments)foo;
// CHECK-LABEL: @end
@objc class AnEnumMethod {
  @objc func takeAndReturnEnum(foo: FooComments) -> NegativeValues {
    return .Zung
  }
}

// CHECK-LABEL: typedef SWIFT_ENUM(unsigned int, ExplicitValues) {
// CHECK-LABEL:   ExplicitValuesZim = 0,
// CHECK-LABEL:   ExplicitValuesZang = 219,
// CHECK-LABEL:   ExplicitValuesZung = 220,
// CHECK-LABEL: };

@objc enum ExplicitValues: CUnsignedInt {
  case Zim, Zang = 219, Zung

  func methodNotExportedToObjC() {}
}

// CHECK-LABEL: /// Foo: A feer, a female feer.
// CHECK-LABEL: typedef SWIFT_ENUM(NSInteger, FooComments) {
// CHECK-LABEL:   /// Zim: A zeer, a female zeer.
// CHECK-LABEL:   FooCommentsZim = 0,
// CHECK-LABEL:   FooCommentsZang = 1,
// CHECK-LABEL:   FooCommentsZung = 2,
// CHECK-LABEL: };

/// Foo: A feer, a female feer.
@objc enum FooComments: Int {
  /// Zim: A zeer, a female zeer.
  case Zim
  case Zang, Zung
}

// CHECK-LABEL: typedef SWIFT_ENUM(int16_t, NegativeValues) {
// CHECK-LABEL:   Zang = -219,
// CHECK-LABEL:   Zung = -218,
// CHECK-LABEL: };
@objc enum NegativeValues: Int16 {
  case Zang = -219, Zung

  func methodNotExportedToObjC() {}
}

// CHECK-NOT: enum {{[A-Z]+}}
// CHECK-LABEL: @interface ZEnumMethod
// CHECK-LABEL: - (enum NegativeValues)takeAndReturnEnum:(enum FooComments)foo;
// CHECK-LABEL: @end
@objc class ZEnumMethod {
  @objc func takeAndReturnEnum(foo: FooComments) -> NegativeValues {
    return .Zung
  }
}

