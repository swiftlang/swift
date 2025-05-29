// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -Xllvm -sil-print-types -emit-sil -swift-version 5 -use-clang-function-types -experimental-print-full-convention -o - | %FileCheck %s --check-prefix=CHECK-%target-ptrsize

// REQUIRES: OS=linux-android ||  OS=linux-androideabi

import ctypes

public func f(g: @convention(c, cType: "void (*)(size_t)") (Int) -> ()) { g(0) }

// CHECK-32: sil @$s4main1f1gyySiXzC9_ZTSPFvjE_tF : $@convention(thin) (@convention(c, cType: "void (*)(unsigned int)") (Int) -> ()) -> () {
// CHECK-32: bb0(%0 : $@convention(c, cType: "void (*)(unsigned int)") (Int) -> ()):
// CHECK-32:   debug_value %0 : $@convention(c, cType: "void (*)(unsigned int)") (Int) -> (), let, name "g", argno 1 // id: %1

// CHECK-64: sil @$s4main1f1gyySiXzC9_ZTSPFvmE_tF : $@convention(thin) (@convention(c, cType: "void (*)(unsigned long)") (Int) -> ()) -> () {
// CHECK-64: bb0(%0 : $@convention(c, cType: "void (*)(unsigned long)") (Int) -> ()):
// CHECK-64:   debug_value %0 : $@convention(c, cType: "void (*)(unsigned long)") (Int) -> (), let, name "g", argno 1 // id: %1
