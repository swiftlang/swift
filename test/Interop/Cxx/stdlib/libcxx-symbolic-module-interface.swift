// Only run this test with older libc++, before the top-level std module got split into multiple top-level modules.
// RUN: %empty-directory(%t)
// RUN: %target-clangxx %S/Inputs/check-libcxx-version.cpp -o %t/check-libcxx-version
// RUN: %target-codesign %t/check-libcxx-version

// RUN: %target-swift-ide-test -print-module -module-to-print=CxxStdlib -source-filename=x -enable-experimental-cxx-interop -enable-objc-interop -module-print-submodules -enable-experimental-feature ImportSymbolicCXXDecls > %t/result.txt
// RUN: %target-run %t/check-libcxx-version || %FileCheck %s --check-prefixes=CHECK,VECTOR --input-file=%t/result.txt
// RUN: %target-run %t/check-libcxx-version || %FileCheck %s --check-prefixes=CHECK,STRING --input-file=%t/result.txt
// RUN: %target-run %t/check-libcxx-version || %FileCheck %s --check-prefixes=CHECK,MAP --input-file=%t/result.txt

// REQUIRES: asserts
// REQUIRES: OS=macosx

// CHECK: enum std {
// CHECK: enum __1 {

// STRING: struct basic_string {

// STRING: typealias string = std.__1.basic_string

// VECTOR: struct vector {
// VECTOR: mutating func push_back(_ __x: Any)
// VECTOR: }

// MAP: struct map {

// CHECK-NOT: enum std
