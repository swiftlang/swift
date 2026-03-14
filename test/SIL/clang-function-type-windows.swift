// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -Xllvm -sil-print-types -emit-sil -swift-version 5 -use-clang-function-types -experimental-print-full-convention -o - | %FileCheck %s

// REQUIRES: OS=windows-msvc

import ctypes

public func f(g: @convention(c, cType: "void (*)(size_t)") (Int) -> ()) { g(0) }

// CHECK: sil @$s4main1f1gyySiXzC9_ZTSPFvyE_tF : $@convention(thin) (@convention(c, cType: "void (*)(unsigned long long)") (Int) -> ()) -> () {
// CHECK: bb0(%0 : $@convention(c, cType: "void (*)(unsigned long long)") (Int) -> ()):
// CHECK:   debug_value %0 : $@convention(c, cType: "void (*)(unsigned long long)") (Int) -> (), let, name "g", argno 1 // id: %1
