// RUN: %target-swift-frontend -parse-as-library -O -module-name=test %s -emit-sil | %FileCheck %s

func closure(_ a: Int, b: Int) -> Bool {
  return a < b
}

// Check that closure() is inlined into call_closure after call_closure is
// specialized for it.

// CHECK-LABEL: sil shared [noinline] @_T04test12call_closureSbSi_SiSbSi_SitctF27_T04test7closureSbSi_Si1btFTf1nnc_n
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

