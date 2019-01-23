// RUN: %target-swift-frontend %s -module-name AutolinkingTest -F %S/Inputs -import-underlying-module -emit-ir | %FileCheck %s

// Linux uses a different autolinking mechanism, based on
// swift-autolink-extract. This file tests the Darwin mechanism.
// UNSUPPORTED: OS=linux-gnu
// UNSUPPORTED: OS=linux-gnueabihf
// UNSUPPORTED: OS=freebsd
// UNSUPPORTED: OS=linux-androideabi

// Use a type declared in the Clang part of the module.
public let y = Test()

// CHECK: !llvm.linker.options
// CHECK-NOT: !{!"-framework", !"AutolinkingTest"}
// CHECK: !{!"-lswiftCore"}
// CHECK-NOT: !{!"-framework", !"AutolinkingTest"}