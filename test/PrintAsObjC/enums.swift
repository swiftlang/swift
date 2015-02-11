// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend %clang-importer-sdk -enable-source-import -emit-module -emit-module-doc -o %t %s -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend %clang-importer-sdk -parse-as-library %t/enums.swiftmodule -parse -emit-objc-header-path %t/enums.h -import-objc-header %S/../Inputs/empty.h -disable-objc-attr-requires-foundation-module
// RUN: FileCheck %s < %t/enums.h
// RUN: FileCheck -check-prefix=NEGATIVE %s < %t/enums.h
// RUN: %check-in-clang %t/enums.h
// RUN: %check-in-clang -fno-modules %t/enums.h -include Foundation.h -include ctypes.h -include CoreFoundation.h

// REQUIRES: objc_interop

import Foundation

// NEGATIVE-NOT: NSMalformedEnumMissingTypedef :
// CHECK-LABEL: enum FooComments : NSInteger;
// CHECK-LABEL: enum NegativeValues : int16_t;

// CHECK-LABEL: @interface AnEnumMethod
// CHECK-NEXT: - (enum NegativeValues)takeAndReturnEnum:(enum FooComments)foo;
// CHECK-NEXT: - (void)acceptPlainEnum:(enum NSMalformedEnumMissingTypedef)_;
// CHECK: @end
@objc class AnEnumMethod {
  @objc func takeAndReturnEnum(foo: FooComments) -> NegativeValues {
    return .Zung
  }
  @objc func acceptPlainEnum(_: NSMalformedEnumMissingTypedef) {}
}

// CHECK-LABEL: typedef SWIFT_ENUM(unsigned int, ExplicitValues) {
// CHECK-NEXT:   ExplicitValuesZim = 0,
// CHECK-NEXT:   ExplicitValuesZang = 219,
// CHECK-NEXT:   ExplicitValuesZung = 220,
// CHECK-NEXT: };

@objc enum ExplicitValues: CUnsignedInt {
  case Zim, Zang = 219, Zung

  func methodNotExportedToObjC() {}
}

// CHECK-LABEL: /// Foo: A feer, a female feer.
// CHECK-NEXT: typedef SWIFT_ENUM(NSInteger, FooComments) {
// CHECK:   /// Zim: A zeer, a female zeer.
// CHECK-NEXT:   FooCommentsZim = 0,
// CHECK-NEXT:   FooCommentsZang = 1,
// CHECK-NEXT:   FooCommentsZung = 2,
// CHECK-NEXT: };

/// Foo: A feer, a female feer.
@objc enum FooComments: Int {
  /// Zim: A zeer, a female zeer.
  case Zim
  case Zang, Zung
}

// CHECK-LABEL: typedef SWIFT_ENUM(int16_t, NegativeValues) {
// CHECK-NEXT:   Zang = -219,
// CHECK-NEXT:   Zung = -218,
// CHECK-NEXT: };
@objc enum NegativeValues: Int16 {
  case Zang = -219, Zung

  func methodNotExportedToObjC() {}
}

// CHECK-NOT: enum {{[A-Z]+}}
// CHECK-LABEL: @interface ZEnumMethod
// CHECK-NEXT: - (enum NegativeValues)takeAndReturnEnum:(enum FooComments)foo;
// CHECK: @end
@objc class ZEnumMethod {
  @objc func takeAndReturnEnum(foo: FooComments) -> NegativeValues {
    return .Zung
  }
}

