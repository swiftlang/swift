// RUN: %target-swift-ide-test -print-module -module-to-print=CxxStdlib -source-filename=x -enable-experimental-cxx-interop -enable-objc-interop -module-print-submodules -enable-experimental-feature ImportSymbolicCXXDecls | %FileCheck %s

// REQUIRES: asserts
// REQUIRES: OS=macosx
// REQUIRES: libcxx-in-sdk

// CHECK: enum std {
// CHECK-NEXT: enum __1 {

// CHECK: struct basic_string {

// CHECK: typealias string = std.__1.basic_string

// CHECK: struct vector {
// CHECK: mutating func push_back()
// CHECK: }

// CHECK: struct map {

// CHECK-NOT: enum std
