// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-source-import -emit-module -emit-module-doc -o %t %s -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %t/enums.swiftmodule -parse -emit-objc-header-path %t/enums.h -import-objc-header %S/../Inputs/empty.h -disable-objc-attr-requires-foundation-module
// RUN: FileCheck %s < %t/enums.h
// RUN: FileCheck -check-prefix=NEGATIVE %s < %t/enums.h
// RUN: %check-in-clang %t/enums.h
// RUN: %check-in-clang -fno-modules -Qunused-arguments %t/enums.h -include Foundation.h -include ctypes.h -include CoreFoundation.h

// REQUIRES: objc_interop

import Foundation

// NEGATIVE-NOT: NSMalformedEnumMissingTypedef :
// NEGATIVE-NOT: enum EnumNamed
// CHECK-LABEL: enum FooComments : NSInteger;
// CHECK-LABEL: enum NegativeValues : int16_t;
// CHECK-LABEL: enum ObjcEnumNamed : NSInteger;

// CHECK-LABEL: @interface AnEnumMethod
// CHECK-NEXT: - (enum NegativeValues)takeAndReturnEnum:(enum FooComments)foo;
// CHECK-NEXT: - (void)acceptPlainEnum:(enum NSMalformedEnumMissingTypedef)_;
// CHECK-NEXT: - (enum ObjcEnumNamed)takeAndReturnRenamedEnum:(enum ObjcEnumNamed)foo;
// CHECK: @end
@objc class AnEnumMethod {
  @objc func takeAndReturnEnum(_ foo: FooComments) -> NegativeValues {
    return .Zung
  }
  @objc func acceptPlainEnum(_: NSMalformedEnumMissingTypedef) {}
  @objc func takeAndReturnRenamedEnum(_ foo: EnumNamed) -> EnumNamed {
    return .A
  }
}

// CHECK-LABEL: typedef SWIFT_ENUM_NAMED(NSInteger, ObjcEnumNamed, "EnumNamed") {
// CHECK-NEXT:   ObjcEnumNamedA = 0,
// CHECK-NEXT:   ObjcEnumNamedB = 1,
// CHECK-NEXT:   ObjcEnumNamedC = 2,
// CHECK-NEXT:   ObjcEnumNamedD = 3,
// CHECK-NEXT:   ObjcEnumNamedHelloDolly = 4,
// CHECK-NEXT: };

@objc(ObjcEnumNamed) enum EnumNamed: Int {
  case A, B, C, d, helloDolly
}

// CHECK-LABEL: typedef SWIFT_ENUM(NSInteger, EnumWithNamedConstants) {
// CHECK-NEXT:   kEnumA SWIFT_COMPILE_NAME("A") = 0,
// CHECK-NEXT:   kEnumB SWIFT_COMPILE_NAME("B") = 1,
// CHECK-NEXT:   kEnumC SWIFT_COMPILE_NAME("C") = 2,
// CHECK-NEXT: };

@objc enum EnumWithNamedConstants: Int {
  @objc(kEnumA) case A
  @objc(kEnumB) case B
  @objc(kEnumC) case C
}

// CHECK-LABEL: typedef SWIFT_ENUM(unsigned int, ExplicitValues) {
// CHECK-NEXT:   ExplicitValuesZim = 0,
// CHECK-NEXT:   ExplicitValuesZang = 219,
// CHECK-NEXT:   ExplicitValuesZung = 220,
// CHECK-NEXT: };
// NEGATIVE-NOT: ExplicitValuesDomain

@objc enum ExplicitValues: CUnsignedInt {
  case Zim, Zang = 219, Zung

  func methodNotExportedToObjC() {}
}

// CHECK: /**
// CHECK-NEXT: Foo: A feer, a female feer.
// CHECK-NEXT: */

// CHECK-NEXT: typedef SWIFT_ENUM(NSInteger, FooComments) {
// CHECK: /**
// CHECK-NEXT: Zim: A zeer, a female zeer.
// CHECK: */
// CHECK-NEXT:   FooCommentsZim = 0,
// CHECK-NEXT:   FooCommentsZang = 1,
// CHECK-NEXT:   FooCommentsZung = 2,
// CHECK-NEXT: };

/// Foo: A feer, a female feer.
@objc public enum FooComments: Int {
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

// CHECK-LABEL: typedef SWIFT_ENUM(NSInteger, SomeErrorProtocol) {
// CHECK-NEXT:   SomeErrorProtocolBadness = 9001,
// CHECK-NEXT:   SomeErrorProtocolWorseness = 9002,
// CHECK-NEXT: };
// CHECK-NEXT: static NSString * _Nonnull const SomeErrorProtocolDomain = @"enums.SomeErrorProtocol";
@objc enum SomeErrorProtocol: Int, ErrorProtocol {
  case Badness = 9001
  case Worseness
}

// CHECK-LABEL: typedef SWIFT_ENUM(NSInteger, SomeOtherErrorProtocol) {
// CHECK-NEXT:   SomeOtherErrorProtocolDomain = 0,
// CHECK-NEXT: };
// NEGATIVE-NOT: NSString * _Nonnull const SomeOtherErrorProtocolDomain
@objc enum SomeOtherErrorProtocol: Int, ErrorProtocol {
  case Domain // collision!
}

// CHECK-LABEL: typedef SWIFT_ENUM_NAMED(NSInteger, ObjcErrorType, "SomeRenamedErrorType") {
// CHECK-NEXT:   ObjcErrorTypeBadStuff = 0,
// CHECK-NEXT: };
// CHECK-NEXT: static NSString * _Nonnull const ObjcErrorTypeDomain = @"enums.SomeRenamedErrorType";
@objc(ObjcErrorType) enum SomeRenamedErrorType: Int, ErrorProtocol {
  case BadStuff
}

// CHECK-NOT: enum {{[A-Z]+}}
// CHECK-LABEL: @interface ZEnumMethod
// CHECK-NEXT: - (enum NegativeValues)takeAndReturnEnum:(enum FooComments)foo;
// CHECK: @end
@objc class ZEnumMethod {
  @objc func takeAndReturnEnum(_ foo: FooComments) -> NegativeValues {
    return .Zung
  }
}

