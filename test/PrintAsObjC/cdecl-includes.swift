/// Print #includes for C clients and reference imported types.
/// This test shouldn't require the objc runtime.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

/// Generate the compatibility header cdecl.h
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %t/Lib.swift \
// RUN:   -emit-module -verify -o %t -I %t \
// RUN:   -import-bridging-header %t/BridgingHeader.h \
// RUN:   -emit-clang-header-path %t/cdecl.h \
// RUN:   -disable-objc-interop \
// RUN:   -enable-experimental-feature CDecl

/// Check compatibility header directly
// RUN: %FileCheck %s --input-file %t/cdecl.h
// RUN: %check-in-clang-c -I %t %t/cdecl.h \
// RUN:   -isysroot %S/../Inputs/clang-importer-sdk

/// Compile a client against the compatibility header
// RUN: %clang-no-modules -c %t/Client.c -I %t -Werror \
// RUN:   -isysroot %S/../Inputs/clang-importer-sdk

// REQUIRES: swift_feature_CDecl

//--- module.modulemap

module CModule {
    header "CModule_FileA.h"
    header "sub/CModule_FileB.h"
}

//--- CModule_FileA.h

struct CStruct { int a; };

//--- sub/CModule_FileB.h

union CUnion { long a; float b; };

//--- Dependency.h

typedef enum TKTimeSetting {
    TKTimeSettingLight,
    TKTimeSettingNormal,
    TKTimeSettingDark
} TKTimeSetting;

//--- BridgingHeader.h

#include "Dependency.h"

//--- Lib.swift

import CModule

// CHECK: #if __has_feature(objc_modules)
// CHECK: @import CModule;
// CHECK-NEXT: #elif defined(__OBJC__)
// CHECK-NEXT: #import <CModule_FileA.h>
// CHECK-NEXT: #import <sub{{[/\\]}}CModule_FileB.h>
// CHECK-NEXT: #else
// CHECK-NEXT: #include <CModule_FileA.h>
// CHECK-NEXT: #include <sub{{[/\\]}}CModule_FileB.h>
// CHECK-NEXT: #endif

// CHECK: #if defined(__OBJC__)
// CHECK: #import "
// CHECK-SAME: BridgingHeader.h"
// CHECK-NEXT: #else
// CHECK-NEXT: #include "
// CHECK-SAME: BridgingHeader.h"
// CHECK-NEXT: #endif

// CHECK-NOT: BridgingHeader

// CHECK: #if defined(__cplusplus)
// CHECK: extern "C" {
// CHECK: #endif

@cdecl("mirror_struct")
public func a_mirrorStruct(_ a: CStruct) -> CStruct { a }
// CHECK: SWIFT_EXTERN struct CStruct mirror_struct(struct CStruct a) SWIFT_NOEXCEPT SWIFT_WARN_UNUSED_RESULT;

@cdecl("mirror_union")
public func b_mirrorStruct(_ a: CUnion) -> CUnion { a }
// CHECK: SWIFT_EXTERN union CUnion mirror_union(union CUnion a) SWIFT_NOEXCEPT SWIFT_WARN_UNUSED_RESULT;


@cdecl("TKGetDefaultToastSetting")
public func c_defaultToastSetting() -> TKTimeSetting { TKTimeSettingNormal } // It would be nice to import TKTimeSettingNormal as a member.
// CHECK: SWIFT_EXTERN TKTimeSetting TKGetDefaultToastSetting(void) SWIFT_NOEXCEPT SWIFT_WARN_UNUSED_RESULT;

// CHECK:      #if defined(__cplusplus)
// CHECK-NEXT: }
// CHECK-NEXT: #endif

//--- Client.c

#include "cdecl.h"

int main() {
  struct CStruct s = { 42 };
  struct CStruct s_out = mirror_struct(s);

  union CUnion u = { 43 };
  union CUnion u_out = mirror_union(u);

  TKTimeSetting def = TKGetDefaultToastSetting();
}
