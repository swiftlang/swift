/// Ensure we print @cdecl and @_cdecl only once.

// RUN: %empty-directory(%t)

/// Generate cdecl.h
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) \
// RUN:   %s -emit-module -verify -o %t -emit-module-doc \
// RUN:   -emit-objc-header-path %t/cdecl.h \
// RUN:   -disable-objc-attr-requires-foundation-module \
// RUN:   -enable-experimental-feature CDecl

/// Check cdecl.h directly
// RUN: %FileCheck %s --input-file %t/cdecl.h
// RUN: %check-in-clang %t/cdecl.h
// RUN: %check-in-clang-c %t/cdecl.h -Wnullable-to-nonnull-conversion
// RUN: %check-in-clang-cxx %t/cdecl.h

// REQUIRES: swift_feature_CDecl
// REQUIRES: objc_interop

@cdecl("cFunc")
func cFunc() { }
// CHECK: cFunc
// CHECK-NOT: cFunc

/// The class would break C parsing if printed in wrong block
@objc
class ObjCClass {}

@_cdecl("objcFunc")
func objcFunc() -> ObjCClass! { return ObjCClass() }
// CHECK: objcFunc
// CHECK-NOT: objcFunc
