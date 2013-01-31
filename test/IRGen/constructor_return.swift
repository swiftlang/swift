// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm | FileCheck %s

// CHECK: [[S:%V18constructor_return1S]] = type { %Si }
// CHECK: [[C:%C18constructor_return1C]] = type { %swift.refcounted, %Sb }

// CHECK: define void [[FOO:@_T18constructor_return3fooFT_T_]]
func foo() {}
// CHECK: define void [[BAR:@_T18constructor_return3barFT_T_]]
func bar() {}

struct S {
  var x:Int
  // CHECK: define i64 @_TV18constructor_return1SCfMS0_FT1bSb_S0_(i1 %b) {
  // CHECK:        call void [[FOO]]
  // CHECK:      if.true:
  // CHECK-NEXT:   br label %return
  // CHECK:      condition.cont:
  // CHECK:        call void [[BAR]]
  // CHECK:        br label %return
  // CHECK:      return:
  // CHECK:        ret i64
  // CHECK: }
  constructor(b:Bool) {
    foo()
    if b { return }
    bar()
  }
}

class C {
  var b:Bool

  // CHECK: define i64 @_TC18constructor_return1CD(%swift.refcounted*) {
  // CHECK:        call void [[FOO]]
  // CHECK:      if.true:
  // CHECK:        br label %cleanup
  // CHECK:      condition.cont:
  // CHECK:        call void [[BAR]]
  // CHECK:        br label %cleanup
  // CHECK:      cleanup:
  // CHECK:        call {{.*}} @swift_release
  // CHECK:      return:
  // CHECK:        ret i64
  // CHECK: }
  destructor {
    foo()
    if b { return }
    bar()
  }

  // CHECK: define %C18constructor_return1C* @_TC18constructor_return1CcfMS0_FT1bSb_S0_(i1 %b, [[C]]* %this) {
  // CHECK:        call void [[FOO]]
  // CHECK:      if.true:
  // CHECK:        br label %cleanup
  // CHECK:      condition.cont:
  // CHECK:        call void [[BAR]]
  // CHECK:        br label %cleanup
  // CHECK:      cleanup:
  // CHECK:      return:
  // CHECK:        ret %C18constructor_return1C*
  // CHECK: }
  constructor(b:Bool) {
    this.b = b
    foo()
    if b { return }
    bar()
  }

}

