// RUN: %empty-directory(%t)

// Test the behavior of printing Unicode.Scalar in the compatibility header.
// This is wrong, it should either be rejected and considered non-representable
// or actually be printed using a C / Objective-C compatible type.

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) \
// RUN:   %s -emit-module -verify -o %t \
// RUN:   -emit-clang-header-path %t/compat.h
// RUN: %FileCheck %s --input-file %t/compat.h

@_cdecl("referencesScalar")
func referencesScalar() -> Unicode.Scalar { fatalError() }
// CHECK: SWIFT_EXTERN Scalar referencesScalar(void)
