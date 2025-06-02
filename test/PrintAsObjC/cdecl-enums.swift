/// Variant of PrintAsObjC/enums.swift for @cdecl enums.

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-source-import \
// RUN:   -emit-module -emit-module-doc -o %t %s \
// RUN:   -import-objc-header %S/Inputs/enums.h \
// RUN:   -emit-objc-header-path %t/enums.h \
// RUN:   -disable-objc-attr-requires-foundation-module \
// RUN:   -enable-experimental-feature CDecl

// RUN: %FileCheck %s --input-file %t/enums.h
// RUN: %FileCheck -check-prefix=NEGATIVE %s --input-file %t/enums.h
// RUN: %check-in-clang %t/enums.h

// REQUIRES: swift_feature_CDecl
// REQUIRES: objc_interop

import Foundation

// NEGATIVE-NOT: enum EnumNamed

/// No error domains in C mode.
// NEGATIVE-NOT: @"main.

// CHECK-LABEL: typedef SWIFT_ENUM_NAMED(ptrdiff_t, ObjcEnumNamed, "EnumNamed", closed) {
// CHECK-NEXT:   ObjcEnumNamedA = 0,
// CHECK-NEXT:   ObjcEnumNamedB = 1,
// CHECK-NEXT:   ObjcEnumNamedC = 2,
// CHECK-NEXT:   ObjcEnumNamedD = 3,
// CHECK-NEXT:   ObjcEnumNamedHelloDolly = 4,
// CHECK-NEXT: };

@cdecl("ObjcEnumNamed") enum EnumNamed: Int {
  case A, B, C, d, helloDolly
}

// CHECK-LABEL: typedef SWIFT_ENUM_NAMED(unsigned int, ExplicitValues, "ExplicitValues", closed) {
// CHECK-NEXT:   ExplicitValuesZim = 0,
// CHECK-NEXT:   ExplicitValuesZang = 219,
// CHECK-NEXT:   ExplicitValuesZung = 220,
// CHECK-NEXT: };
// NEGATIVE-NOT: ExplicitValuesDomain

@cdecl("ExplicitValues") enum ExplicitValues: CUnsignedInt {
  case Zim, Zang = 219, Zung

  func methodNotExportedToC() {}
}

// CHECK: /// Foo: A feer, a female feer.
// CHECK-NEXT: typedef SWIFT_ENUM_NAMED(int, FooComments, "FooComments", closed) {
// CHECK: /// Zim: A zeer, a female zeer.
// CHECK-NEXT:   FooCommentsZim = 0,
// CHECK-NEXT:   FooCommentsZang = 1,
// CHECK-NEXT:   FooCommentsZung = 2,
// CHECK-NEXT: }

/// Foo: A feer, a female feer.
@cdecl("FooComments") public enum FooComments: CInt {
  /// Zim: A zeer, a female zeer.
  case Zim
  case Zang, Zung
}

// CHECK-LABEL: typedef SWIFT_ENUM_NAMED(int16_t, NegativeValues, "NegativeValues", closed) {
// CHECK-NEXT:   Zang = -219,
// CHECK-NEXT:   Zung = -218,
// CHECK-NEXT: };
@cdecl("NegativeValues") enum NegativeValues: Int16 {
  case Zang = -219, Zung

  func methodNotExportedToC() {}
}

// CHECK-LABEL: typedef SWIFT_ENUM_NAMED(ptrdiff_t, SomeError, "SomeError", closed) {
// CHECK-NEXT:   SomeErrorBadness = 9001,
// CHECK-NEXT:   SomeErrorWorseness = 9002,
// CHECK-NEXT: };
@cdecl("SomeError") enum SomeError: Int, Error {
  case Badness = 9001
  case Worseness
}

// CHECK-LABEL: typedef SWIFT_ENUM_NAMED(ptrdiff_t, SomeOtherError, "SomeOtherError", closed) {
// CHECK-NEXT:   SomeOtherErrorDomain = 0,
// CHECK-NEXT: };
@cdecl("SomeOtherError") enum SomeOtherError: Int, Error {
  case Domain
}

// CHECK-LABEL: typedef SWIFT_ENUM_NAMED(ptrdiff_t, ObjcErrorType, "SomeRenamedErrorType", closed) {
// CHECK-NEXT:   ObjcErrorTypeBadStuff = 0,
// CHECK-NEXT: };
@cdecl("ObjcErrorType") enum SomeRenamedErrorType: Int, Error {
  case BadStuff
}

@cdecl("acceptMemberImported") func acceptMemberImported(a: Wrapper.Raw, b: Wrapper.Enum, c: Wrapper.Options, d: Wrapper.Typedef, e: Wrapper.Anon, ee: Wrapper.Anon2) {}
// CHECK-LABEL: SWIFT_EXTERN void acceptMemberImported(enum MemberRaw a, enum MemberEnum b, MemberOptions c, enum MemberTypedef d, MemberAnon e, MemberAnon2 ee) SWIFT_NOEXCEPT;

@cdecl("acceptPlainEnum") func acceptPlainEnum(_: NSMalformedEnumMissingTypedef) {}
// CHECK-LABEL: SWIFT_EXTERN void acceptPlainEnum(enum NSMalformedEnumMissingTypedef) SWIFT_NOEXCEPT;

@cdecl("acceptTopLevelImported") func acceptTopLevelImported(a: TopLevelRaw, b: TopLevelEnum, c: TopLevelOptions, d: TopLevelTypedef, e: TopLevelAnon) {}
// CHECK-LABEL: SWIFT_EXTERN void acceptTopLevelImported(enum TopLevelRaw a, TopLevelEnum b, TopLevelOptions c, TopLevelTypedef d, TopLevelAnon e) SWIFT_NOEXCEPT;

@cdecl("takeAndReturnEnumC") func takeAndReturnEnumC(_ foo: FooComments) -> NegativeValues {
  return .Zung
}
// CHECK-LABEL: SWIFT_EXTERN SWIFT_ENUM_TAG NegativeValues takeAndReturnEnumC(SWIFT_ENUM_TAG FooComments foo) SWIFT_NOEXCEPT SWIFT_WARN_UNUSED_RESULT;

@cdecl("takeAndReturnRenamedEnum") func takeAndReturnRenamedEnum(_ foo: EnumNamed) -> EnumNamed {
  return .A
}
// CHECK-LABEL: SWIFT_EXTERN SWIFT_ENUM_TAG ObjcEnumNamed takeAndReturnRenamedEnum(SWIFT_ENUM_TAG ObjcEnumNamed foo) SWIFT_NOEXCEPT SWIFT_WARN_UNUSED_RESULT;

/// Objective-C user.

// CHECK-LABEL: SWIFT_ENUM_FWD_DECL(int, FooComments)
// CHECK-LABEL: SWIFT_ENUM_FWD_DECL(int16_t, NegativeValues)

// CHECK-LABEL: SWIFT_EXTERN SWIFT_ENUM_TAG NegativeValues takeAndReturnEnumObjC(SWIFT_ENUM_TAG FooComments foo) SWIFT_NOEXCEPT SWIFT_WARN_UNUSED_RESULT;
@_cdecl("takeAndReturnEnumObjC") func takeAndReturnEnumObjC(_ foo: FooComments) -> NegativeValues {
  return .Zung
}
