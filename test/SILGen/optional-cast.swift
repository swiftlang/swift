// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

class A {}
class B : A {}

// CHECK-LABEL: sil hidden @_T04main3fooyAA1ACSgF : $@convention(thin) (@owned Optional<A>) -> () {
// CHECK:    bb0([[ARG:%.*]] : $Optional<A>):
// CHECK:      [[X:%.*]] = alloc_box ${ var Optional<B> }, var, name "x"
// CHECK-NEXT: [[PB:%.*]] = project_box [[X]]
//   Check whether the temporary holds a value.
// CHECK:      [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:      [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
// CHECK:      [[T1:%.*]] = select_enum [[ARG_COPY]]
// CHECK-NEXT: cond_br [[T1]], [[IS_PRESENT:bb.*]], [[NOT_PRESENT:bb[0-9]+]]
//
// CHECK:    [[NOT_PRESENT]]:
// CHECK:      end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK:      br [[NOT_PRESENT_FINISH:bb[0-9]+]]
//
//   If so, pull the value out and check whether it's a B.
// CHECK:    [[IS_PRESENT]]:
// CHECK-NEXT: [[VAL:%.*]] = unchecked_enum_data [[ARG_COPY]] : $Optional<A>, #Optional.some!enumelt.1
// CHECK-NEXT: [[X_VALUE:%.*]] = init_enum_data_addr [[PB]] : $*Optional<B>, #Optional.some
// CHECK-NEXT: checked_cast_br [[VAL]] : $A to $B, [[IS_B:bb.*]], [[NOT_B:bb[0-9]+]]
//
//   If so, materialize that and inject it into x.
// CHECK:    [[IS_B]]([[T0:%.*]] : $B):
// CHECK-NEXT: store [[T0]] to [init] [[X_VALUE]] : $*B
// CHECK-NEXT: inject_enum_addr [[PB]] : $*Optional<B>, #Optional.some
// CHECK-NEXT: br [[CONT:bb[0-9]+]]
//
//   If not, destroy_value the A and inject nothing into x.
// CHECK:    [[NOT_B]]:
// CHECK-NEXT: destroy_value [[VAL]]
// CHECK-NEXT: inject_enum_addr [[PB]] : $*Optional<B>, #Optional.none
// CHECK-NEXT: br [[CONT]]
//
//   Finish the present path.
// CHECK:    [[CONT]]:
// CHECK-NEXT: end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK-NEXT: br [[RETURN_BB:bb[0-9]+]]
//
//   Finish.
// CHECK:    [[RETURN_BB]]:
// CHECK-NEXT: destroy_value [[X]]
// CHECK-NEXT: destroy_value [[ARG]]
// CHECK-NEXT: tuple
// CHECK-NEXT: return
//
//   Finish the not-present path.
// CHECK:    [[NOT_PRESENT_FINISH]]:
// CHECK-NEXT: inject_enum_addr [[PB]] {{.*}}none
// CHECK-NEXT: br [[RETURN_BB]]
func foo(_ y : A?) {
  var x = (y as? B)
}

// CHECK-LABEL: sil hidden @_T04main3baryAA1ACSgSgSgSgF : $@convention(thin) (@owned Optional<Optional<Optional<Optional<A>>>>) -> () {
// CHECK:    bb0([[ARG:%.*]] : $Optional<Optional<Optional<Optional<A>>>>):
// CHECK:      [[X:%.*]] = alloc_box ${ var Optional<Optional<Optional<B>>> }, var, name "x"
// CHECK-NEXT: [[PB:%.*]] = project_box [[X]]
// -- Check for some(...)
// CHECK-NEXT: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK-NEXT: [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
// CHECK:      [[T1:%.*]] = select_enum [[ARG_COPY]]
// CHECK-NEXT: cond_br [[T1]], [[P:bb.*]], [[NIL_DEPTH_1:bb[0-9]+]]
//
// CHECK: [[NIL_DEPTH_1]]:
// CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK:   br [[FINISH_NIL_0:bb[0-9]+]]
//
//   If so, drill down another level and check for some(some(...)).
// CHECK:    [[P]]:
// CHECK-NEXT: [[VALUE_OOOA:%.*]] = unchecked_enum_data [[ARG_COPY]]
// CHECK:      [[T1:%.*]] = select_enum [[VALUE_OOOA]]
// CHECK-NEXT: cond_br [[T1]], [[PP:bb.*]], [[NIL_DEPTH_2:bb[0-9]+]]
//
// CHECK: [[NIL_DEPTH_2]]:
// CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK:   br [[FINISH_NIL_0]]
//
//   If so, drill down another level and check for some(some(some(...))).
// CHECK:    [[PP]]:
// CHECK-NEXT: [[VALUE_OOA:%.*]] = unchecked_enum_data [[VALUE_OOOA]]
// CHECK:      [[T1:%.*]] = select_enum [[VALUE_OOA]]
// CHECK-NEXT: cond_br [[T1]], [[PPP:bb.*]], [[NIL_DEPTH_3:bb[0-9]+]]
//
// CHECK: [[NIL_DEPTH_3]]:
// CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK:   br [[FINISH_NIL_1:bb[0-9]+]]
//
//   If so, drill down another level and check for some(some(some(some(...)))).
// CHECK:    [[PPP]]:
// CHECK-NEXT: [[VALUE_OA:%.*]] = unchecked_enum_data [[VALUE_OOA]]
// CHECK:      [[T1:%.*]] = select_enum [[VALUE_OA]]
// CHECK-NEXT: cond_br [[T1]], [[PPPP:bb.*]], [[NIL_DEPTH_4:bb[0-9]+]]
//
// CHECK: [[NIL_DEPTH_4]]:
// CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK:   br [[FINISH_NIL_2:bb[0-9]+]]
//
//   If so, pull out the A and check whether it's a B.
// CHECK:    [[PPPP]]:
// CHECK-NEXT: [[VAL:%.*]] = unchecked_enum_data [[VALUE_OA]]
// CHECK-NEXT: checked_cast_br [[VAL]] : $A to $B, [[IS_B:bb.*]], [[NOT_B:bb[0-9]+]]
//
//   If so, inject it back into an optional.
//   TODO: We're going to switch back out of this; we really should peephole it.
// CHECK:    [[IS_B]]([[T0:%.*]] : $B):
// CHECK-NEXT: enum $Optional<B>, #Optional.some!enumelt.1, [[T0]]
// CHECK-NEXT: br [[SWITCH_OB2:bb[0-9]+]](
//
//   If not, inject nothing into an optional.
// CHECK:    [[NOT_B]]:
// CHECK-NEXT: destroy_value [[VAL]]
// CHECK-NEXT: enum $Optional<B>, #Optional.none!enumelt
// CHECK-NEXT: br [[SWITCH_OB2]](
//
//   Switch out on the value in [[OB2]].
// CHECK:    [[SWITCH_OB2]]([[VAL:%[0-9]+]] : $Optional<B>):
// CHECK:    [[T0:%.*]] = select_enum [[VAL]]
// CHECK:    cond_br [[T0]], [[HAVE_B:bb[0-9]+]], [[FINISH_NIL_4:bb[0-9]+]]
//
// CHECK:    [[FINISH_NIL_4]]:
// CHECK:      end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK:      br [[FINISH_NIL_0]]
//
// CHECK:    [[HAVE_B]]:
// CHECK:      [[UNWRAPPED_VAL:%.*]] = unchecked_enum_data [[VAL]]
// CHECK:      [[REWRAPPED_VAL:%.*]] = enum $Optional<B>, #Optional.some!enumelt.1, [[UNWRAPPED_VAL]]
// CHECK:      end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK:      br [[DONE_DEPTH0:bb[0-9]+]]
//
// CHECK:    [[DONE_DEPTH0]](
// CHECK-NEXT: enum $Optional<Optional<B>>, #Optional.some!enumelt.1,
// CHECK-NEXT: br [[DONE_DEPTH1:bb[0-9]+]]
//
//   Set X := some(OOB).
// CHECK:    [[DONE_DEPTH1]]
// CHECK-NEXT: enum $Optional<Optional<Optional<B>>>, #Optional.some!enumelt.1,
// CHECK: br [[DONE_DEPTH2:bb[0-9]+]]
// CHECK:    [[DONE_DEPTH2]]
// CHECK-NEXT: destroy_value [[X]]
// CHECK-NEXT: destroy_value %0
// CHECK:      return
//
//   On various failure paths, set OOB := nil.
// CHECK:    [[FINISH_NIL_2]]:
// CHECK-NEXT: enum $Optional<B>, #Optional.none!enumelt
// CHECK-NEXT: br [[DONE_DEPTH0]]
//
// CHECK:    [[FINISH_NIL_1]]:
// CHECK-NEXT: enum $Optional<Optional<B>>, #Optional.none!enumelt
// CHECK-NEXT: br [[DONE_DEPTH1]]
//
//   On various failure paths, set X := nil.
// CHECK:    [[FINISH_NIL_0]]:
// CHECK-NEXT: inject_enum_addr [[PB]] {{.*}}none
// CHECK-NEXT: br [[DONE_DEPTH2]]
//   Done.
func bar(_ y : A????) {
  var x = (y as? B??)
}


// CHECK-LABEL: sil hidden @_T04main3bazys9AnyObject_pSgF : $@convention(thin) (@owned Optional<AnyObject>) -> () {
// CHECK:       bb0([[ARG:%.*]] : $Optional<AnyObject>):
// CHECK:         [[X:%.*]] = alloc_box ${ var Optional<B> }, var, name "x"
// CHECK-NEXT:    [[PB:%.*]] = project_box [[X]]
// CHECK-NEXT:    [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK-NEXT:    [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
// CHECK:         [[T1:%.*]] = select_enum [[ARG_COPY]]
// CHECK:       bb1:
// CHECK:         [[VAL:%.*]] = unchecked_enum_data [[ARG_COPY]]
// CHECK-NEXT:    [[X_VALUE:%.*]] = init_enum_data_addr [[PB]] : $*Optional<B>, #Optional.some
// CHECK-NEXT:    checked_cast_br [[VAL]] : $AnyObject to $B, [[IS_B:bb.*]], [[NOT_B:bb[0-9]+]]
// CHECK:       [[IS_B]](
// CHECK:       [[NOT_B]]:
// CHECK:         destroy_value [[VAL]]
// CHECK: } // end sil function '_T04main3bazys9AnyObject_pSgF'
func baz(_ y : AnyObject?) {
  var x = (y as? B)
}


// <rdar://problem/17013042> T! <-> T? conversions should not produce a diamond

// CHECK-LABEL: sil hidden @_T04main07opt_to_B8_trivialSQySiGSiSgF
// CHECK:       bb0(%0 : $Optional<Int>):
// CHECK-NEXT:  debug_value %0 : $Optional<Int>, let, name "x"
// CHECK-NEXT:  return %0 : $Optional<Int>
// CHECK-NEXT:}
func opt_to_opt_trivial(_ x: Int?) -> Int! {
  return x
}

// CHECK-LABEL: sil hidden @_T04main07opt_to_B10_referenceAA1CCSgSQyADGF :
// CHECK:  bb0([[ARG:%.*]] : $Optional<C>):
// CHECK:    debug_value [[ARG]] : $Optional<C>, let, name "x"
// CHECK:    [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:    [[RESULT:%.*]] = copy_value [[BORROWED_ARG]]
// CHECK:    destroy_value [[ARG]]
// CHECK:    return [[RESULT]] : $Optional<C>
// CHECK: } // end sil function '_T04main07opt_to_B10_referenceAA1CCSgSQyADGF'
func opt_to_opt_reference(_ x : C!) -> C? { return x }

// CHECK-LABEL: sil hidden @_T04main07opt_to_B12_addressOnly{{[_0-9a-zA-Z]*}}F
// CHECK:       bb0(%0 : $*Optional<T>, %1 : $*Optional<T>):
// CHECK-NEXT:  debug_value_addr %1 : $*Optional<T>, let, name "x"
// CHECK-NEXT:  copy_addr %1 to [initialization] %0
// CHECK-NEXT:  destroy_addr %1
func opt_to_opt_addressOnly<T>(_ x : T!) -> T? { return x }

class C {}

public struct TestAddressOnlyStruct<T> {
  func f(_ a : T?) {}
  
  // CHECK-LABEL: sil hidden @_T04main21TestAddressOnlyStructV8testCall{{[_0-9a-zA-Z]*}}F
  // CHECK: bb0(%0 : $*Optional<T>, %1 : $TestAddressOnlyStruct<T>):
  // CHECK: [[TMPBUF:%.*]] = alloc_stack $Optional<T>
  // CHECK-NEXT: copy_addr %0 to [initialization] [[TMPBUF]]
  // CHECK-NEXT: apply {{.*}}<T>([[TMPBUF]], %1)
  func testCall(_ a : T!) {
    f(a)
  }
}

// CHECK-LABEL: sil hidden @_T04main35testContextualInitOfNonAddrOnlyTypeySiSgF
// CHECK: bb0(%0 : $Optional<Int>):
// CHECK-NEXT: debug_value %0 : $Optional<Int>, let, name "a"
// CHECK-NEXT: [[X:%.*]] = alloc_box ${ var Optional<Int> }, var, name "x"
// CHECK-NEXT: [[PB:%.*]] = project_box [[X]]
// CHECK-NEXT: store %0 to [trivial] [[PB]] : $*Optional<Int>
// CHECK-NEXT: destroy_value [[X]] : ${ var Optional<Int> }
func testContextualInitOfNonAddrOnlyType(_ a : Int?) {
  var x: Int! = a
}
