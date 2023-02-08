// RUN: %target-swift-ide-test -print-module -module-to-print=CxxStdlib -source-filename=x -enable-experimental-cxx-interop -enable-objc-interop -module-print-submodules | %FileCheck %s

// REQUIRES: OS=macosx

// CHECK: enum std {
// CHECK-NEXT: enum __1 {

// CHECK: typealias string =

// CHECK-NOT: enum std
