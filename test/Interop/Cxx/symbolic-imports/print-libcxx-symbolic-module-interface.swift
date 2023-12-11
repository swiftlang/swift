// RUN: rm -rf %t
// RUN: %target-swift-ide-test -print-module -module-to-print=CxxStdlib -source-filename=x -enable-experimental-cxx-interop -enable-objc-interop -module-print-submodules -enable-experimental-feature ImportSymbolicCXXDecls > %t
// RUN: %FileCheck %s --check-prefixes=CHECK,VECTOR --input-file=%t
// RUN: %FileCheck %s --check-prefixes=CHECK,STRING --input-file=%t
// RUN: %FileCheck %s --check-prefixes=CHECK,MAP --input-file=%t

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
