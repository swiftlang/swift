
// RUN: %target-swift-emit-silgen -module-name switch_isa %s | %FileCheck %s

func markUsed<T>(_ t: T) {}

// rdar://17772217
func testSwitchOnExistential(_ value: Any) {
  switch value {
    case true as Bool: markUsed("true")
    case false as Bool: markUsed("false")
    default: markUsed("default")
  }
}

// CHECK-LABEL: sil hidden @$S10switch_isa23testSwitchOnExistentialyyypF :
// CHECK:   [[ANY:%.*]] = alloc_stack $Any
// CHECK:   copy_addr %0 to [initialization] [[ANY]]
// CHECK:   [[BOOL:%.*]] = alloc_stack $Bool
// CHECK:   checked_cast_addr_br copy_on_success Any in [[ANY]] : $*Any to Bool in [[BOOL]] : $*Bool, [[IS_BOOL:bb[0-9]+]], [[IS_NOT_BOOL:bb[0-9]+]]
// CHECK: [[IS_BOOL]]:
// CHECK:   [[T0:%.*]] = load [trivial] [[BOOL]]

enum Foo {
  case A
}
enum Bar<T> {
  case B(T)
}
func testSwitchEnumOnExistential(_ value: Any) {
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

// CHECK-LABEL: sil hidden @$S10switch_isa27testSwitchEnumOnExistentialyyypF : $@convention(thin) (@in_guaranteed Any) -> ()
// CHECK:   checked_cast_addr_br copy_on_success Any in {{%.*}} : $*Any to Foo
// CHECK:   checked_cast_addr_br copy_on_success Any in {{%.*}} : $*Any to Bar<Int>
// CHECK:   checked_cast_addr_br copy_on_success Any in {{%.*}} : $*Any to Bar<Foo>

class B {}
class D: B {}

func guardFn(_ l: D, _ r: D) -> Bool { return true }

// rdar://problem/21087371
// CHECK-LABEL: sil hidden @$S10switch_isa32testSwitchTwoIsPatternsWithGuard_1ryAA1BC_AEtF
// CHECK:         checked_cast_br {{%.*}} : $B to $D, [[R_CAST_YES:bb[0-9]+]], [[R_CAST_NO:bb[0-9]+]]
// CHECK:       [[R_CAST_YES]]({{.*}}):
// CHECK:         checked_cast_br {{%.*}} : $B to $D, [[L_CAST_YES:bb[0-9]+]], [[L_CAST_NO:bb[0-9]+]]
// CHECK:       [[L_CAST_YES]]({{.*}}):
// CHECK:         function_ref @$S10switch_isa7guardFnySbAA1DC_ADtF
// CHECK:         cond_br {{%.*}}, [[GUARD_YES:bb[0-9]+]], [[GUARD_NO:bb[0-9]+]]
// CHECK:       [[GUARD_NO]]:
// CHECK-NEXT:    destroy_value [[R2:%.*]] : $D
// CHECK-NEXT:    destroy_value [[L2:%.*]] : $D
// CHECK-NEXT:    br [[CONT:bb[0-9]+]]
// CHECK:       [[L_CAST_NO]]:
// CHECK-NEXT:    destroy_value [[R2:%.*]] : $D
// CHECK-NEXT:    br [[CONT]]
func testSwitchTwoIsPatternsWithGuard(_ l: B, r: B) {
  switch (l, r) {
  case (let l2 as D, let r2 as D) where guardFn(l2, r2):
    break
  default:
    break
  }
}
