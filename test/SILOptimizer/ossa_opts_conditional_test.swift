// RUN: %target-swift-frontend -O -emit-sil  %s | %FileCheck %s
class C {
}

struct X {
  let c1: C
  let c2: C
}

@inline(never)
func createit() -> X {
  return X(c1: C(), c2: C())
}

@inline(never)
func useit(_ c: C) {
  print(c)
}

// CHECK-LABEL: sil hidden [noinline] @$s26ossa_opts_conditional_test6testityySbF : $@convention(thin) (Bool) -> () {
// CHECK-NOT: retain
// CHECK-LABEL: } // end sil function '$s26ossa_opts_conditional_test6testityySbF'
@inline(never)
func testit(_ b: Bool) {
  let x: X
  let c: C

  if b {
    x = createit()
    c = x.c1
  } else {
    x = createit()
    c = x.c2
  }
  useit(c)
}

testit(true)
