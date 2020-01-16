// RUN: %target-swift-frontend %s -emit-sil -swift-version 5 -use-clang-function-types -experimental-print-full-convention -o - | %FileCheck %s

public func f(g: @convention(c) () -> ()) { g() }

// CHECK: sil @$s4main1f1gyyyXC_tF : $@convention(thin) (@convention(c, cType: "void (*)(void)") @noescape () -> ()) -> () {
// CHECK: bb0(%0 : $@convention(c, cType: "void (*)(void)") @noescape () -> ()):
// CHECK:   debug_value %0 : $@convention(c, cType: "void (*)(void)") @noescape () -> (), let, name "g", argno 1 // id: %1
// CHECK:   %2 = apply %0() : $@convention(c, cType: "void (*)(void)") @noescape () -> ()
