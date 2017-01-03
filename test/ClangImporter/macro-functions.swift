// Other ClangImporter tests verify that function-like C macros are imported
// into Swift so that identifiers are resolved correctly. This test verifies
// these can be used in executables, and don't crash at runtime.

// REQUIRES: executable_test

// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift %s -import-objc-header %S/Inputs/macro-functions.h -o %t/macro-functions
// RUN: %target-run %t/macro-functions | %FileCheck %s

print("INT_MACRO_FUNC: \(INT_MACRO_FUNC())") // CHECK: INT_MACRO_FUNC: 1
print("STRING_MACRO_FUNC: \(STRING_MACRO_FUNC())") // CHECK: STRING_MACRO_FUNC: foo

