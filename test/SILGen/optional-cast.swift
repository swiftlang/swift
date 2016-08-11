// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

class A {}
class B : A {}


// CHECK-LABEL: sil hidden @_TF4main3foo
// CHECK:      [[X:%.*]] = alloc_box $Optional<B>, var, name "x"
// CHECK-NEXT: [[PB:%.*]] = project_box [[X]]
//   Check whether the temporary holds a value.
// CHECK:      [[T1:%.*]] = select_enum %0
// CHECK-NEXT: cond_br [[T1]], [[IS_PRESENT:bb.*]], [[NOT_PRESENT:bb[0-9]+]]
//   If so, pull the value out and check whether it's a B.
// CHECK:    [[IS_PRESENT]]:
// CHECK-NEXT: [[VAL:%.*]] = unchecked_enum_data %0 : $Optional<A>, #Optional.some!enumelt.1
// CHECK-NEXT: [[X_VALUE:%.*]] = init_enum_data_addr [[PB]] : $*Optional<B>, #Optional.some
// CHECK-NEXT: checked_cast_br [[VAL]] : $A to $B, [[IS_B:bb.*]], [[NOT_B:bb[0-9]+]]
//   If so, materialize that and inject it into x.
// CHECK:    [[IS_B]]([[T0:%.*]] : $B):
// CHECK-NEXT: store [[T0]] to [[X_VALUE]] : $*B
// CHECK-NEXT: inject_enum_addr [[PB]] : $*Optional<B>, #Optional.some
// CHECK-NEXT: br [[CONT:bb[0-9]+]]
//   If not, release the A and inject nothing into x.
// CHECK:    [[NOT_B]]:
// CHECK-NEXT: strong_release [[VAL]]
// CHECK-NEXT: inject_enum_addr [[PB]] : $*Optional<B>, #Optional.none
// CHECK-NEXT: br [[CONT]]
//   Finish the present path.
// CHECK:    [[CONT]]:
// CHECK-NEXT: br [[CONT2:bb[0-9]+]]
//   Finish.
// CHECK:    [[CONT2]]:
// CHECK-NEXT: strong_release [[X]]
// CHECK-NEXT: release_value %0
//   Finish the not-present path.
// CHECK:    [[NOT_PRESENT]]:
// CHECK-NEXT: inject_enum_addr [[PB]] {{.*}}none
// CHECK-NEXT: br [[CONT2]]
func foo(_ y : A?) {
  var x = (y as? B)
}

// CHECK-LABEL: sil hidden @_TF4main3bar
// CHECK:      [[X:%.*]] = alloc_box $Optional<Optional<Optional<B>>>, var, name "x"
// CHECK-NEXT: [[PB:%.*]] = project_box [[X]]

// Check for some(...)
// CHECK-NEXT: retain_value %0
// CHECK:      [[T1:%.*]] = select_enum %0
// CHECK-NEXT: cond_br [[T1]], [[P:bb.*]], [[NIL_DEPTH2:bb[0-9]+]]
//   If so, drill down another level and check for some(some(...)).
// CHECK:    [[P]]:
// CHECK-NEXT: [[VALUE_OOOA:%.*]] = unchecked_enum_data %0
// CHECK:      [[T1:%.*]] = select_enum [[VALUE_OOOA]]
// CHECK-NEXT: cond_br [[T1]], [[PP:bb.*]], [[NIL_DEPTH2:bb[0-9]+]]
//   If so, drill down another level and check for some(some(some(...))).
// CHECK:    [[PP]]:
// CHECK-NEXT: [[VALUE_OOA:%.*]] = unchecked_enum_data [[VALUE_OOOA]]
// CHECK:      [[T1:%.*]] = select_enum [[VALUE_OOA]]
// CHECK-NEXT: cond_br [[T1]], [[PPP:bb.*]], [[NIL_DEPTH1:bb[0-9]+]]
//   If so, drill down another level and check for some(some(some(some(...)))).
// CHECK:    [[PPP]]:
// CHECK-NEXT: [[VALUE_OA:%.*]] = unchecked_enum_data [[VALUE_OOA]]
// CHECK:      [[T1:%.*]] = select_enum [[VALUE_OA]]
// CHECK-NEXT: cond_br [[T1]], [[PPPP:bb.*]], [[NIL_DEPTH0:bb[0-9]+]]
//   If so, pull out the A and check whether it's a B.
// CHECK:    [[PPPP]]:
// CHECK-NEXT: [[VAL:%.*]] = unchecked_enum_data [[VALUE_OA]]
// CHECK-NEXT: checked_cast_br [[VAL]] : $A to $B, [[IS_B:bb.*]], [[NOT_B:bb[0-9]+]]
//   If so, inject it back into an optional.
//   TODO: We're going to switch back out of this; we really should peephole it.
// CHECK:    [[IS_B]]([[T0:%.*]] : $B):
// CHECK-NEXT: enum $Optional<B>, #Optional.some!enumelt.1, [[T0]]
// CHECK-NEXT: br [[SWITCH_OB2:bb[0-9]+]](
//   If not, inject nothing into an optional.
// CHECK:    [[NOT_B]]:
// CHECK-NEXT: strong_release [[VAL]]
// CHECK-NEXT: enum $Optional<B>, #Optional.none!enumelt
// CHECK-NEXT: br [[SWITCH_OB2]](
//   Switch out on the value in [[OB2]].
// CHECK:    [[SWITCH_OB2]]([[VAL:%[0-9]+]] : $Optional<B>):
// CHECK:      br [[DONE_DEPTH0:bb[0-9]+]]
// CHECK:    [[DONE_DEPTH0]](
// CHECK-NEXT: enum $Optional<Optional<B>>, #Optional.some!enumelt.1,
// CHECK-NEXT: br [[DONE_DEPTH1:bb[0-9]+]]
//   Set X := some(OOB).
// CHECK:    [[DONE_DEPTH1]]
// CHECK-NEXT: enum $Optional<Optional<Optional<B>>>, #Optional.some!enumelt.1,
// CHECK: br [[DONE_DEPTH2:bb[0-9]+]]
// CHECK:    [[DONE_DEPTH2]]
// CHECK-NEXT: strong_release [[X]]
// CHECK-NEXT: release_value %0
// CHECK:      return
//   On various failure paths, set OOB := nil.
// CHECK:    [[NIL_DEPTH1]]:
// CHECK-NEXT: enum $Optional<Optional<B>>, #Optional.none!enumelt
// CHECK-NEXT: br [[DONE_DEPTH1]]
//   On various failure paths, set X := nil.
// CHECK:    [[NIL_DEPTH2]]:
// CHECK-NEXT: inject_enum_addr [[PB]] {{.*}}none
// CHECK-NEXT: br [[DONE_DEPTH2]]
//   Done.
func bar(_ y : A????) {
  var x = (y as? B??)
}

// CHECK-LABEL: sil hidden @_TF4main3baz
// CHECK:      [[X:%.*]] = alloc_box $Optional<B>, var, name "x"
// CHECK-NEXT: [[PB:%.*]] = project_box [[X]]
// CHECK-NEXT: retain_value %0
// CHECK:      [[T1:%.*]] = select_enum %0
// CHECK: [[VAL:%.*]] = unchecked_enum_data %0
// CHECK-NEXT: [[X_VALUE:%.*]] = init_enum_data_addr [[PB]] : $*Optional<B>, #Optional.some
// CHECK-NEXT: checked_cast_br [[VAL]] : $AnyObject to $B, [[IS_B:bb.*]], [[NOT_B:bb[0-9]+]]
func baz(_ y : AnyObject?) {
  var x = (y as? B)
}


// <rdar://problem/17013042> T! <-> T? conversions should not produce a diamond

// CHECK-LABEL: sil hidden @_TF4main18opt_to_opt_trivialFGSqSi_GSQSi_
// CHECK:       bb0(%0 : $Optional<Int>):
// CHECK-NEXT:  debug_value %0 : $Optional<Int>, let, name "x"
// CHECK-NEXT:  %2 = unchecked_trivial_bit_cast %0 : $Optional<Int> to $ImplicitlyUnwrappedOptional<Int>
// CHECK-NEXT:  return %2 : $ImplicitlyUnwrappedOptional<Int>
// CHECK-NEXT:}
func opt_to_opt_trivial(_ x: Int?) -> Int! {
  return x
}

// CHECK-LABEL: sil hidden @_TF4main20opt_to_opt_referenceFGSQCS_1C_GSqS0__
// CHECK:       bb0(%0 : $ImplicitlyUnwrappedOptional<C>):
// CHECK-NEXT:  debug_value %0 : $ImplicitlyUnwrappedOptional<C>, let, name "x"
// CHECK-NEXT:  %2 = unchecked_ref_cast %0 : $ImplicitlyUnwrappedOptional<C> to $Optional<C>
// CHECK-NEXT:  return %2 : $Optional<C>
// CHECK-NEXT:}
func opt_to_opt_reference(_ x : C!) -> C? { return x }

// CHECK-LABEL: sil hidden @_TF4main22opt_to_opt_addressOnly
// CHECK:       bb0(%0 : $*Optional<T>, %1 : $*ImplicitlyUnwrappedOptional<T>):
// CHECK-NEXT:  debug_value_addr %1 : $*ImplicitlyUnwrappedOptional<T>, let, name "x"
// CHECK-NEXT:  %3 = unchecked_addr_cast %0 : $*Optional<T> to $*ImplicitlyUnwrappedOptional<T>
// CHECK-NEXT:  copy_addr [take] %1 to [initialization] %3
func opt_to_opt_addressOnly<T>(_ x : T!) -> T? { return x }

class C {}

public struct TestAddressOnlyStruct<T> {
  func f(_ a : T?) {}
  
  // CHECK-LABEL: sil hidden @_TFV4main21TestAddressOnlyStruct8testCall
  // CHECK: bb0(%0 : $*ImplicitlyUnwrappedOptional<T>, %1 : $TestAddressOnlyStruct<T>):
  // CHECK: [[TMPBUF:%.*]] = alloc_stack $Optional<T>
  // CHECK: [[TMPCAST:%.*]] = unchecked_addr_cast [[TMPBUF]]
  // CHECK-NEXT: copy_addr %0 to [initialization] [[TMPCAST]]
  // CHECK-NEXT: apply {{.*}}<T>([[TMPBUF]], %1)
  func testCall(_ a : T!) {
    f(a)
  }
}

// CHECK-LABEL: sil hidden @_TF4main35testContextualInitOfNonAddrOnlyTypeFGSqSi_T_
// CHECK: bb0(%0 : $Optional<Int>):
// CHECK-NEXT: debug_value %0 : $Optional<Int>, let, name "a"
// CHECK-NEXT: [[X:%.*]] = alloc_box $ImplicitlyUnwrappedOptional<Int>, var, name "x"
// CHECK-NEXT: [[PB:%.*]] = project_box [[X]]
// CHECK-NEXT: [[CAST:%.*]] = unchecked_addr_cast [[PB]] : $*ImplicitlyUnwrappedOptional<Int> to $*Optional<Int>
// CHECK-NEXT: store %0 to [[CAST]] : $*Optional<Int>
// CHECK-NEXT: strong_release [[X]] : $@box ImplicitlyUnwrappedOptional<Int>
func testContextualInitOfNonAddrOnlyType(_ a : Int?) {
  var x: Int! = a
}
