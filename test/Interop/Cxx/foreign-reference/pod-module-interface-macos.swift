// RUN: %target-swift-ide-test -print-module -module-to-print=POD -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop -target %target-arch-apple-macos11 | %FileCheck %s

// REQUIRES: OS=macosx

// CHECK: @available(macOS 13.3.0, *)
// CHECK-NEXT: class Empty {

// CHECK: @available(macOS 13.3.0, *)
// CHECK-NEXT: class MultipleAttrs {
