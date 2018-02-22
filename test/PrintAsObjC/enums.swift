// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-source-import -emit-module -emit-module-doc -o %t %s -import-objc-header %S/Inputs/enums.h -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %t/enums.swiftmodule -typecheck -emit-objc-header-path %t/enums.h -import-objc-header %S/Inputs/enums.h -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck %s < %t/enums.h
// RUN: %FileCheck -check-prefix=NEGATIVE %s < %t/enums.h
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
// CHECK-NEXT: - (enum NegativeValues)takeAndReturnEnum:(enum FooComments)foo SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (void)acceptPlainEnum:(enum NSMalformedEnumMissingTypedef)_;
// CHECK-NEXT: - (enum ObjcEnumNamed)takeAndReturnRenamedEnum:(enum ObjcEnumNamed)foo SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (void)acceptTopLevelImportedWithA:(enum TopLevelRaw)a b:(TopLevelEnum)b c:(TopLevelOptions)c d:(TopLevelTypedef)d e:(TopLevelAnon)e;
// CHECK-NEXT: - (void)acceptMemberImportedWithA:(enum MemberRaw)a b:(enum MemberEnum)b c:(MemberOptions)c d:(enum MemberTypedef)d e:(MemberAnon)e ee:(MemberAnon2)ee;
// CHECK: @end
@objc class AnEnumMethod {
  @objc func takeAndReturnEnum(_ foo: FooComments) -> NegativeValues {
    return .Zung
  }
  @objc func acceptPlainEnum(_: NSMalformedEnumMissingTypedef) {}
  @objc func takeAndReturnRenamedEnum(_ foo: EnumNamed) -> EnumNamed {
    return .A
  }

  @objc func acceptTopLevelImported(a: TopLevelRaw, b: TopLevelEnum, c: TopLevelOptions, d: TopLevelTypedef, e: TopLevelAnon) {}
  @objc func acceptMemberImported(a: Wrapper.Raw, b: Wrapper.Enum, c: Wrapper.Options, d: Wrapper.Typedef, e: Wrapper.Anon, ee: Wrapper.Anon2) {}
}

// CHECK-LABEL: typedef SWIFT_ENUM_NAMED(NSInteger, ObjcEnumNamed, "EnumNamed", closed) {
// CHECK-NEXT:   ObjcEnumNamedA = 0,
// CHECK-NEXT:   ObjcEnumNamedB = 1,
// CHECK-NEXT:   ObjcEnumNamedC = 2,
// CHECK-NEXT:   ObjcEnumNamedD = 3,
// CHECK-NEXT:   ObjcEnumNamedHelloDolly = 4,
// CHECK-NEXT: };

@objc(ObjcEnumNamed) enum EnumNamed: Int {
  case A, B, C, d, helloDolly
}

// CHECK-LABEL: typedef SWIFT_ENUM(NSInteger, EnumWithNamedConstants, closed) {
// CHECK-NEXT:   kEnumA SWIFT_COMPILE_NAME("A") = 0,
// CHECK-NEXT:   kEnumB SWIFT_COMPILE_NAME("B") = 1,
// CHECK-NEXT:   kEnumC SWIFT_COMPILE_NAME("C") = 2,
// CHECK-NEXT: };

@objc enum EnumWithNamedConstants: Int {
  @objc(kEnumA) case A
  @objc(kEnumB) case B
  @objc(kEnumC) case C
}

// CHECK-LABEL: typedef SWIFT_ENUM(unsigned int, ExplicitValues, closed) {
// CHECK-NEXT:   ExplicitValuesZim = 0,
// CHECK-NEXT:   ExplicitValuesZang = 219,
// CHECK-NEXT:   ExplicitValuesZung = 220,
// CHECK-NEXT: };
// NEGATIVE-NOT: ExplicitValuesDomain

@objc enum ExplicitValues: CUnsignedInt {
  case Zim, Zang = 219, Zung

  func methodNotExportedToObjC() {}
}

// CHECK: /// Foo: A feer, a female feer.
// CHECK-NEXT: typedef SWIFT_ENUM(NSInteger, FooComments, closed) {
// CHECK: /// Zim: A zeer, a female zeer.
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

// CHECK-LABEL: typedef SWIFT_ENUM(int16_t, NegativeValues, closed) {
// CHECK-NEXT:   Zang = -219,
// CHECK-NEXT:   Zung = -218,
// CHECK-NEXT: };
@objc enum NegativeValues: Int16 {
  case Zang = -219, Zung

  func methodNotExportedToObjC() {}
}

// CHECK-LABEL: typedef SWIFT_ENUM(NSInteger, SomeError, closed) {
// CHECK-NEXT:   SomeErrorBadness = 9001,
// CHECK-NEXT:   SomeErrorWorseness = 9002,
// CHECK-NEXT: };
// CHECK-NEXT: static NSString * _Nonnull const SomeErrorDomain = @"enums.SomeError";
@objc enum SomeError: Int, Error {
  case Badness = 9001
  case Worseness
}

// CHECK-LABEL: typedef SWIFT_ENUM(NSInteger, SomeOtherError, closed) {
// CHECK-NEXT:   SomeOtherErrorDomain = 0,
// CHECK-NEXT: };
// NEGATIVE-NOT: NSString * _Nonnull const SomeOtherErrorDomain
@objc enum SomeOtherError: Int, Error {
  case Domain // collision!
}

// CHECK-LABEL: typedef SWIFT_ENUM_NAMED(NSInteger, ObjcErrorType, "SomeRenamedErrorType", closed) {
// CHECK-NEXT:   ObjcErrorTypeBadStuff = 0,
// CHECK-NEXT: };
// CHECK-NEXT: static NSString * _Nonnull const ObjcErrorTypeDomain = @"enums.SomeRenamedErrorType";
@objc(ObjcErrorType) enum SomeRenamedErrorType: Int, Error {
  case BadStuff
}

// CHECK-NOT: enum {{[A-Z]+}}
// CHECK-LABEL: @interface ZEnumMethod
// CHECK-NEXT: - (enum NegativeValues)takeAndReturnEnum:(enum FooComments)foo SWIFT_WARN_UNUSED_RESULT;
// CHECK: @end
@objc class ZEnumMethod {
  @objc func takeAndReturnEnum(_ foo: FooComments) -> NegativeValues {
    return .Zung
  }
}
