// RUN: %target-swift-frontend -emit-sil %s -Xllvm -sil-disable-pass=AllocBoxToStack -Xllvm -sil-disable-pass-only-function='$s26testflag_disable_pass_only4foo1yyF' | %FileCheck %s

// CHECK-LABEL: sil hidden [noinline] @$s26testflag_disable_pass_only4foo1yyF :
// CHECK: alloc_box 
// CHECK-LABEL: } // end sil function '$s26testflag_disable_pass_only4foo1yyF'
@inline(never)
func foo1() {
  var a = 100
  @inline(never)
  func bar() {
    print(a)
  }
}

// CHECK-LABEL: sil hidden [noinline] @$s26testflag_disable_pass_only4foo2yyF :
// CHECK-NOT: alloc_box 
// CHECK-LABEL: } // end sil function '$s26testflag_disable_pass_only4foo2yyF'
@inline(never)
func foo2() {
  var a = 100
  @inline(never)
  func bar() {
    print(a)
  }
}
