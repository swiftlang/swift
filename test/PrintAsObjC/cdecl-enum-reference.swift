// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Build CoreLib defining a @cdecl enum.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) \
// RUN:   %t/CoreLib.swift -emit-module -verify -o %t \
// RUN:   -emit-clang-header-path %t/CoreLib.h \
// RUN:   -enable-experimental-feature CDecl
// RUN: %check-in-clang-c %t/CoreLib.h -I %t

/// Build MiddleLib using the @cdecl enum in API.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) \
// RUN:   %t/MiddleLib.swift -emit-module -verify -o %t -I %t \
// RUN:   -emit-clang-header-path %t/MiddleLib.h \
// RUN:   -enable-experimental-feature CDecl
// RUN: %FileCheck %s --input-file %t/MiddleLib.h
// RUN: %check-in-clang-c %t/MiddleLib.h -I %t

/// Build a client.
// RUN: %clang-no-modules -c %t/Client.c -I %t \
// RUN:   -F %S/../Inputs/clang-importer-sdk-path/frameworks \
// RUN:   -I %clang-include-dir -Werror \
// RUN:   -isysroot %S/../Inputs/clang-importer-sdk

// REQUIRES: swift_feature_CDecl

//--- CoreLib.swift
@cdecl("CEnum")
public enum CEnum: CInt { case A, B }

//--- MiddleLib.swift
import CoreLib

@cdecl("CFunc")
public func CFunc(e: CEnum) {}
// CHECK: typedef SWIFT_ENUM_FWD_DECL(int, CEnum)
// CHECK: SWIFT_EXTERN void CFunc(SWIFT_ENUM_TAG CEnum e) SWIFT_NOEXCEPT;

//--- Client.c
#include "CoreLib.h"
#include "MiddleLib.h"

int main() {
    CFunc(CEnumA);
}
