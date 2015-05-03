// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

class A {}
class B : A {}

func foo(y : A?) {
  var x = (y as? B)
}
// CHECK-DAG: sil hidden @_TF4main3foo
// CHECK:      [[X:%.*]] = alloc_box $Optional<B> // var x
//   Check whether the temporary holds a value.
// CHECK:      [[T1:%.*]] = select_enum %0
// CHECK-NEXT: cond_br [[T1]], [[IS_PRESENT:bb.*]], [[NOT_PRESENT:bb[0-9]+]]
//   If not, destroy the temporary.
// CHECK:    [[NOT_PRESENT]]:
// CHECK-NEXT: release_value %0
// CHECK-NEXT: br [[NOT_PRESENT:bb[0-9]+]]
//   If so, pull the value out and check whether it's a B.
// CHECK:    [[IS_PRESENT]]:
// CHECK-NEXT: [[VAL:%.*]] = unchecked_enum_data %0 : $Optional<A>, #Optional.Some!enumelt.1
// CHECK-NEXT: [[X_VALUE:%.*]] = init_enum_data_addr [[X]]#1 : $*Optional<B>, #Optional.Some
// CHECK-NEXT: checked_cast_br [[VAL]] : $A to $B, [[IS_B:bb.*]], [[NOT_B:bb[0-9]+]]
//   If so, materialize that and inject it into x.
// CHECK:    [[IS_B]]([[T0:%.*]] : $B):
// CHECK-NEXT: store [[T0]] to [[X_VALUE]] : $*B
// CHECK-NEXT: inject_enum_addr [[X]]#1 : $*Optional<B>, #Optional.Some
// CHECK-NEXT: br [[CONT:bb[0-9]+]]
//   If not, release the A and inject nothing into x.
// CHECK:    [[NOT_B]]:
// CHECK-NEXT: strong_release [[VAL]]
// CHECK-NEXT: inject_enum_addr [[X]]#1 : $*Optional<B>, #Optional.None
// CHECK-NEXT: br [[CONT]]
//   Finish the present path.
// CHECK:    [[CONT]]:
// CHECK-NEXT: br [[CONT2:bb[0-9]+]]
//   Finish the not-present path.
// CHECK:    [[NOT_PRESENT]]:
// CHECK-NEXT: inject_enum_addr [[X]]{{.*}}None
// CHECK-NEXT: br [[CONT2]]
//   Finish.
// CHECK:    [[CONT2]]:
// CHECK-NEXT: strong_release [[X]]#0
// CHECK-NEXT: release_value %0

func bar(y : A????) {
  var x = (y as? B??)
}
// CHECK-DAG: sil hidden @_TF4main3bar
// CHECK:      [[X:%.*]] = alloc_box $Optional<Optional<Optional<B>>> // var x

// Check for Some(...)
// CHECK-NEXT: retain_value %0
// CHECK:      [[T1:%.*]] = select_enum %0
// CHECK-NEXT: cond_br [[T1]], [[P:bb.*]], [[NP:bb[0-9]+]]
//   If not, finish the evaluation.
// CHECK:    [[NP]]:
// CHECK-NEXT: release_value %0
// CHECK-NEXT: br [[NIL_DEPTH2:bb[0-9]+]]
//   If so, drill down another level and check for Some(Some(...)).
// CHECK:    [[P]]:
// CHECK-NEXT: [[VALUE_OOOA:%.*]] = unchecked_enum_data %0
// CHECK:      [[T1:%.*]] = select_enum [[VALUE_OOOA]]
// CHECK-NEXT: cond_br [[T1]], [[PP:bb.*]], [[NPP:bb[0-9]+]]
//   If not, finish the evaluation.
// CHECK:    [[NPP]]:
// CHECK-NEXT: release_value [[VALUE_OOOA]]
// CHECK-NEXT: br [[NIL_DEPTH2]]
//   If so, drill down another level and check for Some(Some(Some(...))).
// CHECK:    [[PP]]:
// CHECK-NEXT: [[VALUE_OOA:%.*]] = unchecked_enum_data [[VALUE_OOOA]]
// CHECK:      [[T1:%.*]] = select_enum [[VALUE_OOA]]
// CHECK-NEXT: cond_br [[T1]], [[PPP:bb.*]], [[NPPP:bb[0-9]+]]
//   If not, wrap up with Some(None).
// CHECK:    [[NPPP]]:
// CHECK-NEXT: release_value [[VALUE_OOA]]
// CHECK-NEXT: br [[NIL_DEPTH1:bb[0-9]+]]
//   If so, drill down another level and check for Some(Some(Some(Some(...)))).
// CHECK:    [[PPP]]:
// CHECK-NEXT: [[VALUE_OA:%.*]] = unchecked_enum_data [[VALUE_OOA]]
// CHECK:      [[T1:%.*]] = select_enum [[VALUE_OA]]
// CHECK-NEXT: cond_br [[T1]], [[PPPP:bb.*]], [[NPPPP:bb[0-9]+]]
//   If not, wrap up with Some(Some(None)).
// CHECK:    [[NPPPP]]:
// CHECK-NEXT: release_value [[VALUE_OA]]
// CHECK-NEXT: br [[NIL_DEPTH0:bb[0-9]+]]
//   If so, pull out the A and check whether it's a B.
// CHECK:    [[PPPP]]:
// CHECK-NEXT: [[VAL:%.*]] = unchecked_enum_data [[VALUE_OA]]
// CHECK-NEXT: checked_cast_br [[VAL]] : $A to $B, [[IS_B:bb.*]], [[NOT_B:bb[0-9]+]]
//   If so, inject it back into an optional.
//   TODO: We're going to switch back out of this; we really should peephole it.
// CHECK:    [[IS_B]]([[T0:%.*]] : $B):
// CHECK-NEXT: enum $Optional<B>, #Optional.Some!enumelt.1, [[T0]]
// CHECK-NEXT: br [[SWITCH_OB2:bb[0-9]+]](
//   If not, inject nothing into an optional.
// CHECK:    [[NOT_B]]:
// CHECK-NEXT: strong_release [[VAL]]
// CHECK-NEXT: enum $Optional<B>, #Optional.None!enumelt
// CHECK-NEXT: br [[SWITCH_OB2]](
//   Switch out on the value in [[OB2]].
// CHECK:    [[SWITCH_OB2]]([[VAL:%[0-9]+]] : $Optional<B>):
// CHECK:      [[T1:%.*]] = select_enum [[VAL]]
// CHECK-NEXT: cond_br [[T1]], [[IS_B2:bb.*]], [[NOT_B2:bb[0-9]+]]
//   If it's not present, finish the evaluation.
// CHECK:    [[NOT_B2]]:
// CHECK-NEXT: release_value [[VAL]]
// CHECK-NEXT: br [[NIL_DEPTH2]]
//   If it's present, set OB := Some(x).
// CHECK:    [[IS_B2]]:
// CHECK-NEXT: [[VALUE_B:%.*]] = unchecked_enum_data [[VAL]]
// CHECK-NEXT: enum $Optional<B>, #Optional.Some!enumelt.1, [[VALUE_B]]
// CHECK-NEXT: br [[DONE_DEPTH0:bb[0-9]+]](
//   On various failure paths, set OB := nil.
// CHECK:    [[NIL_DEPTH0]]:
// CHECK-NEXT: enum $Optional<B>, #Optional.None!enumelt
// CHECK-NEXT: br [[DONE_DEPTH0]]
//   Set OOB := Some(OB).
// CHECK:    [[DONE_DEPTH0]]
// CHECK-NEXT: enum $Optional<Optional<B>>, #Optional.Some!enumelt.1,
// CHECK-NEXT: br [[DONE_DEPTH1:bb[0-9]+]]
//   On various failure paths, set OOB := nil.
// CHECK:    [[NIL_DEPTH1]]:
// CHECK-NEXT: enum $Optional<Optional<B>>, #Optional.None!enumelt
// CHECK-NEXT: br [[DONE_DEPTH1]]
//   Set X := Some(OOB).
// CHECK:    [[DONE_DEPTH1]]
// CHECK-NEXT: enum $Optional<Optional<Optional<B>>>, #Optional.Some!enumelt.1,
// CHECK: br [[DONE_DEPTH2:bb[0-9]+]]
//   On various failure paths, set X := nil.
// CHECK:    [[NIL_DEPTH2]]:
// CHECK-NEXT: inject_enum_addr [[X]]{{.*}}None
// CHECK-NEXT: br [[DONE_DEPTH2]]
//   Done.
// CHECK:    [[DONE_DEPTH2]]
// CHECK-NEXT: strong_release [[X]]#0
// CHECK-NEXT: release_value %0

func baz(y : AnyObject?) {
  var x = (y as? B)
}
// CHECK-DAG: sil hidden @_TF4main3baz
// CHECK:      [[X:%.*]] = alloc_box $Optional<B>  // var x
// CHECK-NEXT: retain_value %0
// CHECK:      [[T1:%.*]] = select_enum %0
// CHECK: [[VAL:%.*]] = unchecked_enum_data %0
// CHECK-NEXT: [[X_VALUE:%.*]] = init_enum_data_addr [[X]]#1 : $*Optional<B>, #Optional.Some
// CHECK-NEXT: checked_cast_br [[VAL]] : $AnyObject to $B, [[IS_B:bb.*]], [[NOT_B:bb[0-9]+]]
