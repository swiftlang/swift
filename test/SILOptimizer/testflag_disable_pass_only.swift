// RUN: %target-swift-frontend -emit-sil %s -Xllvm -sil-disable-pass=OnoneSimplification -Xllvm -sil-disable-pass-only-function='$s26testflag_disable_pass_only4foo1SiyF' | %FileCheck %s

// CHECK-LABEL: sil hidden [noinline] @$s26testflag_disable_pass_only4foo1SiyF :
// CHECK:         br bb1
// CHECK-LABEL: } // end sil function '$s26testflag_disable_pass_only4foo1SiyF'
@inline(never)
func foo1() -> Int {
  let a = 100
  if a < 10 {
    return 0
  }
  return 10
}

// CHECK-LABEL: sil hidden [noinline] @$s26testflag_disable_pass_only4foo2SiyF :
// CHECK-NOT:     bb1
// CHECK-LABEL: } // end sil function '$s26testflag_disable_pass_only4foo2SiyF'
@inline(never)
func foo2() -> Int {
  let a = 100
  if a < 10 {
    return 0
  }
  return 10
}
