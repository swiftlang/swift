// RUN: %empty-directory(%t)
// RUN: split-file %S/string-conversions.cpp %t

// RUN: %target-swift-frontend -enable-experimental-feature Embedded %t/print-string.swift -target arm64-apple-macos15.0 -module-name Stringer -enable-experimental-cxx-interop -typecheck -verify -emit-clang-header-path %t/Stringer.h

// Verify the generated header uses embedded mangling ($e prefix, not $s).
// RUN: %FileCheck %s --check-prefix=CHECK-HEADER < %t/Stringer.h

// Verify the generated header compiles as valid C++.
// RUN: %target-interop-build-clangxx -target arm64-apple-macos15.0 -std=gnu++20 -c %t/string-conversions.cpp -I %t -o %t/swift-stdlib-execution.o

// REQUIRES: OS=macosx
// REQUIRES: executable_test
// REQUIRES: embedded_stdlib
// REQUIRES: swift_feature_Embedded

// CHECK-HEADER: __EmbeddedSwift__
// CHECK-HEADER: $eSS7cStringSSSPys4Int8VG_tcfC
// CHECK-HEADER-NOT: $sSS7cStringSSSPys4Int8VG_tcfC
// CHECK-HEADER-NOT: $sSS10FoundationE
