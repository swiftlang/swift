// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

func markUsed<T>(t: T) {}

// rdar://17772217
func testSwitchOnExistential(value: Any) {
  switch value {
    case true as Bool: markUsed("true")
    case false as Bool: markUsed("false")
    default: markUsed("default")
  }
}

// CHECK-LABEL: sil hidden @_TF10switch_isa23testSwitchOnExistentialFP_T_ :
// CHECK:   [[ANY:%.*]] = alloc_stack $protocol<>
// CHECK:   copy_addr %0 to [initialization] [[ANY]]#1
// CHECK:   [[BOOL:%.*]] = alloc_stack $Bool
// CHECK:   checked_cast_addr_br copy_on_success protocol<> in [[ANY]]#1 : $*protocol<> to Bool in [[BOOL]]#1 : $*Bool, [[IS_BOOL:bb[0-9]+]], [[IS_NOT_BOOL:bb[0-9]+]]
// CHECK: [[IS_BOOL]]:
// CHECK:   [[T0:%.*]] = load [[BOOL]]#1

enum Foo {
  case A
}
enum Bar<T> {
  case B(T)
}
func testSwitchEnumOnExistential(value: Any) {
  switch value {
  case Foo.A:
    ()
  case Bar<Int>.B(let i):
    ()
  case Bar<Foo>.B(let f):
    ()
  default:
    ()
  }
}

// CHECK-LABEL: sil hidden @_TF10switch_isa27testSwitchEnumOnExistentialFP_T_ : $@convention(thin) (@in protocol<>) -> ()
// CHECK:   checked_cast_addr_br copy_on_success protocol<> in {{%.*}} : $*protocol<> to Foo
// CHECK:   checked_cast_addr_br copy_on_success protocol<> in {{%.*}} : $*protocol<> to Bar<Int>
// CHECK:   checked_cast_addr_br copy_on_success protocol<> in {{%.*}} : $*protocol<> to Bar<Foo>

class B {}
class D: B {}

func guardFn(l: D, _ r: D) -> Bool { return true }

// rdar://problem/21087371
// CHECK-LABEL: sil hidden @_TF10switch_isa32testSwitchTwoIsPatternsWithGuardFTCS_1B1rS0__T_
// CHECK:         checked_cast_br {{%.*}} : $B to $D, [[R_CAST_YES:bb[0-9]+]], [[R_CAST_NO:bb[0-9]+]]
// CHECK:       [[R_CAST_YES]]({{.*}}):
// CHECK:         checked_cast_br {{%.*}} : $B to $D, [[L_CAST_YES:bb[0-9]+]], [[L_CAST_NO:bb[0-9]+]]
// CHECK:       [[L_CAST_YES]]({{.*}}):
// CHECK:         function_ref @_TF10switch_isa7guardFnFTCS_1DS0__Sb
// CHECK:         cond_br {{%.*}}, [[GUARD_YES:bb[0-9]+]], [[GUARD_NO:bb[0-9]+]]
// CHECK:       [[GUARD_NO]]:
// CHECK-NEXT:    strong_release [[R2:%.*]] : $D
// CHECK-NEXT:    strong_release [[L2:%.*]] : $D
// CHECK-NEXT:    br [[CONT:bb[0-9]+]]
// CHECK:       [[L_CAST_NO]]:
// CHECK-NEXT:    strong_release [[R2:%.*]] : $D
// CHECK-NEXT:    br [[CONT]]
func testSwitchTwoIsPatternsWithGuard(l: B, r: B) {
  switch (l, r) {
  case (let l2 as D, let r2 as D) where guardFn(l2, r2):
    break
  default:
    break
  }
}
