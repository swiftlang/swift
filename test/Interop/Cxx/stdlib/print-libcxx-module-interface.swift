// RUN: %target-swift-ide-test -print-module -module-to-print=std -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop -enable-objc-interop -module-print-submodules | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: libcxx-in-sdk

// CHECK: enum std {
// CHECK-NEXT: enum __1 {

// CHECK: typealias string =

// CHECK-NOT: enum std
