// RUN: %target-swift-frontend -parse-as-library -O -import-objc-header %S/Inputs/bitfield.h -module-name=test %s -Xllvm -sil-print-types -emit-sil | %FileCheck %s

// REQUIRES: swift_in_compiler

var gg: Int = {
  print("gg init")
  return 27
}()

// Test that the compiler doesn't crash with a global C bitfield.
var bitfield = S(a: 0, b: 0)

// CHECK-LABEL: sil @$s4test3cseSiyF
// CHECK:   builtin "once"
// CHECK-NOT:   builtin "once"
// CHECK:   [[G:%[0-9]+]] = load
// CHECK-NOT:   builtin "once"
// CHECK:   builtin "sadd_{{.*}}"([[G]] : $Builtin.Int{{[0-9]+}}, [[G]] : $Builtin.Int{{[0-9]+}}, %{{[0-9]+}} : $Builtin.Int1)
// CHECK-NOT:   builtin "once"
// CHECK: } // end sil function '$s4test3cseSiyF'
public func cse() -> Int {
  return gg + gg
}

// CHECK-LABEL: sil @$s4test4licmSiyF
// CHECK: bb0:
// CHECK:   builtin "once"
// CHECK: bb1{{.*}}:
// CHECK-NOT:   builtin "once"
// CHECK: } // end sil function '$s4test4licmSiyF'
public func licm() -> Int {
  var s = 0
  for _ in 0..<100 {
    s += gg
  }
  return s
}

