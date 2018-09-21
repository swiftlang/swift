// RUN: %target-swift-frontend -parse-as-library -O -module-name=test %s -emit-sil | %FileCheck %s

func closure(_ a: Int, b: Int) -> Bool {
  return a < b
}

// Check that closure() is inlined into call_closure after call_closure is
// specialized for it.

// CHECK-LABEL: sil shared [noinline] @$s4test12call_closureySbSi_SiSbSi_SitXEtF27$s4test7closure_1bSbSi_SitFTf1nnc_n
// CHECK-NOT: apply
// CHECK: builtin "cmp_slt_Int
// CHECK-NOT: apply
// CHECK: return
@inline(never)
func call_closure(_ a: Int, _ b: Int, _ f: (Int , Int) -> Bool) -> Bool {
  return f(a, b)
}

public func testit() -> Bool {
  return call_closure(0, 1, closure)
}

