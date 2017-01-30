// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

func markUsed<T>(_ t: T) {}

// rdar://17772217
func testSwitchOnExistential(_ value: Any) {
  switch value {
    case true as Bool: markUsed("true")
    case false as Bool: markUsed("false")
    default: markUsed("default")
  }
}

// CHECK-LABEL: sil hidden @_T010switch_isa23testSwitchOnExistentialyypF :
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

// CHECK-LABEL: sil hidden @_T010switch_isa27testSwitchEnumOnExistentialyypF : $@convention(thin) (@in Any) -> ()
// CHECK:   checked_cast_addr_br copy_on_success Any in {{%.*}} : $*Any to Foo
// CHECK:   checked_cast_addr_br copy_on_success Any in {{%.*}} : $*Any to Bar<Int>
// CHECK:   checked_cast_addr_br copy_on_success Any in {{%.*}} : $*Any to Bar<Foo>

class B {}
class D: B {}

func guardFn(_ l: D, _ r: D) -> Bool { return true }

// rdar://problem/21087371
// CHECK-LABEL: sil hidden @_T010switch_isa32testSwitchTwoIsPatternsWithGuardyAA1BC_AD1rtF : $@convention(thin) (@owned B, @owned B) -> () {
// CHECK: bb0([[ARG1:%.*]] : $B, [[ARG2:%.*]] : $B):
// CHECK:    [[BORROWED_ARG1:%.*]] = begin_borrow [[ARG1]]
// CHECK:    [[COPIED_ARG1:%.*]] = copy_value [[BORROWED_ARG1]]
// CHECK:    [[BORROWED_ARG2:%.*]] = begin_borrow [[ARG2]]
// CHECK:    [[COPIED_ARG2:%.*]] = copy_value [[BORROWED_ARG2]]
// CHECK:    [[TUPLE:%.*]] = tuple ([[COPIED_ARG1]] : $B, [[COPIED_ARG2]] : $B)
// CHECK:    [[BORROWED_TUPLE:%.*]] = begin_borrow [[TUPLE]]
// CHECK:    [[L:%.*]] = tuple_extract [[BORROWED_TUPLE]] : $(B, B), 0
// CHECK:    [[L_COPY:%.*]] = copy_value [[L]]
// CHECK:    end_borrow [[BORROWED_TUPLE]] from [[TUPLE]]
// CHECK:    [[BORROWED_TUPLE:%.*]] = begin_borrow [[TUPLE]]
// CHECK:    [[R:%.*]] = tuple_extract [[BORROWED_TUPLE]] : $(B, B), 1
// CHECK:    [[R_COPY:%.*]] = copy_value [[R]]
// CHECK:    end_borrow [[BORROWED_TUPLE]] from [[TUPLE]]
// CHECK:    checked_cast_br [[R_COPY]] : $B to $D, [[R_CAST_YES:bb[0-9]+]], [[R_CAST_NO:bb[0-9]+]]
//
// CHECK: [[R_CAST_YES]]([[R_COPY_FORWARDED:%.*]] : $D):
// CHECK:    [[R2:%.*]] = copy_value [[R_COPY_FORWARDED]]
// CHECK:    checked_cast_br [[L_COPY]]  : $B to $D, [[L_CAST_YES:bb[0-9]+]], [[L_CAST_NO:bb[0-9]+]]
//
// CHECK: [[L_CAST_YES]]([[L_COPY_FORWARDED:%.*]] : $D):
// CHECK:    [[L2:%.*]] = copy_value [[L_COPY_FORWARDED]]
// CHECK:    function_ref @_T010switch_isa7guardFnSbAA1DC_ADtF
// CHECK:    [[BORROWED_L2:%.*]] = begin_borrow [[L2]]
// CHECK:    [[L2_COPY:%.*]] = copy_value [[BORROWED_L2]]
// CHECK:    [[BORROWED_R2:%.*]] = begin_borrow [[R2]]
// CHECK:    [[R2_COPY:%.*]] = copy_value [[BORROWED_R2]]
// CHECK:    apply {{.*}}([[L2_COPY]], [[R2_COPY]]) : $@convention(thin) (@owned D, @owned D)
// CHECK:    end_borrow [[BORROWED_R2]] from [[R2]]
// CHECK:    end_borrow [[BORROWED_L2]] from [[L2]]
// CHECK:    cond_br {{%.*}}, [[GUARD_YES:bb[0-9]+]], [[GUARD_NO:bb[0-9]+]]
//
// CHECK: [[GUARD_YES]]:
// CHECK-NEXT:    destroy_value [[R2]]
// CHECK-NEXT:    destroy_value [[L2]]
// CHECK-NEXT:    destroy_value [[R_COPY]]
// CHECK-NEXT:    destroy_value [[L_COPY]]
// CHECK-NEXT:    destroy_value [[TUPLE]]
// CHECK-NEXT:    end_borrow [[BORROWED_ARG2]] from [[ARG2]]
// CHECK-NEXT:    end_borrow [[BORROWED_ARG1]] from [[ARG1]]
// CHECK-NEXT:    br [[EPILOG_BB:bb[0-9]+]]
//
// CHECK: [[GUARD_NO]]:
// CHECK-NEXT:    destroy_value [[R2:%.*]] : $D
// CHECK-NEXT:    destroy_value [[L2:%.*]] : $D
// CHECK-NEXT:    destroy_value [[R_COPY]]
// CHECK-NEXT:    destroy_value [[L_COPY]]
// CHECK-NEXT:    br [[CONT:bb[0-9]+]]
//
// CHECK: [[L_CAST_NO]]:
// CHECK-NEXT:    destroy_value [[R2:%.*]] : $D
// CHECK-NEXT:    destroy_value [[R_COPY]]
// CHECK-NEXT:    destroy_value [[L_COPY]]
// CHECK-NEXT:    br [[CONT]]
//
// CHECK: [[R_CAST_NO]]:
// CHECK-NEXT:    destroy_value [[R_COPY]]
// CHECK-NEXT:    destroy_value [[L_COPY]]
// CHECK-NEXT:    br [[CONT]]
//
// CHECK: [[CONT]]:
// CHECK-NEXT:    destroy_value [[TUPLE]]
// CHECK-NEXT:    end_borrow [[BORROWED_ARG2]] from [[ARG2]]
// CHECK-NEXT:    end_borrow [[BORROWED_ARG1]] from [[ARG1]]
// CHECK-NEXT:    br [[EPILOG_BB]]
//
// CHECK: [[EPILOG_BB]]:
// CHECK-NEXT: destroy_value [[ARG2]]
// CHECK-NEXT: destroy_value [[ARG1]]
func testSwitchTwoIsPatternsWithGuard(_ l: B, r: B) {
  switch (l, r) {
  case (let l2 as D, let r2 as D) where guardFn(l2, r2):
    break
  default:
    break
  }
}
