// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

class A {}
class B : A {}

func foo(y : A?) {
  var x = (y as? B)
}
// CHECK-DAG: sil hidden @_TF4main3foo
// CHECK:      [[X:%.*]] = alloc_box $Optional<B>
//   Materialize the parameter.
// CHECK-NEXT: [[TMP_OPTA:%.*]] = alloc_stack $Optional<A>
// CHECK-NEXT: retain_value %0
// CHECK-NEXT: store %0 to [[TMP_OPTA]]#1
//   Check whether the temporary holds a value.
// CHECK:      [[T1:%.*]] = select_enum_addr [[TMP_OPTA]]#1
// CHECK-NEXT: cond_br [[T1]], [[IS_PRESENT:bb.*]], [[NOT_PRESENT:bb[0-9]+]]
//   If not, destroy the temporary.
// CHECK:    [[NOT_PRESENT]]:
// CHECK-NEXT: destroy_addr [[TMP_OPTA]]#1
// CHECK-NEXT: dealloc_stack [[TMP_OPTA]]#0
// CHECK-NEXT: br [[NOT_PRESENT:bb[0-9]+]]
//   If so, pull the value out and check whether it's a B.
// CHECK:    [[IS_PRESENT]]:
// CHECK-NEXT: [[TMP_A:%.*]] = unchecked_take_enum_data_addr [[TMP_OPTA]]#1
// CHECK-NEXT: [[VAL:%.*]] = load [[TMP_A]]
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
//   Finish the prsent path.
// CHECK:    [[CONT]]:
// CHECK-NEXT: dealloc_stack [[TMP_OPTA]]#0
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
// CHECK:      [[X:%.*]] = alloc_box $Optional<Optional<Optional<B>>>
// CHECK-NEXT: [[TMP_OOB:%.*]] = init_enum_data_addr [[X]]
// CHECK-NEXT: [[TMP_OB:%.*]] = init_enum_data_addr [[TMP_OOB]]
// CHECK-NEXT: [[TMP_B:%.*]] = init_enum_data_addr [[TMP_OB]]
// CHECK-NEXT: [[TMP_OB2:%.*]] = alloc_stack $Optional<B>
// CHECK-NEXT: [[TMP_OA:%.*]] = alloc_stack $Optional<A>
// CHECK-NEXT: [[TMP_OOA:%.*]] = alloc_stack $Optional<Optional<A>>
// CHECK-NEXT: [[TMP_OOOA:%.*]] = alloc_stack $Optional<Optional<Optional<A>>>
// CHECK-NEXT: [[TMP_OOOOA:%.*]] = alloc_stack $Optional<Optional<Optional<Optional<A>>>>
//   Materialize the argument and check for Some(...)
// CHECK-NEXT: retain_value %0
// CHECK-NEXT: store %0 to [[TMP_OOOOA]]#1
// CHECK:      [[T1:%.*]] = select_enum_addr [[TMP_OOOOA]]#1
// CHECK-NEXT: cond_br [[T1]], [[P:bb.*]], [[NP:bb[0-9]+]]
//   If not, finish the evaluation.
// CHECK:    [[NP]]:
// CHECK-NEXT: destroy_addr [[TMP_OOOOA]]#1
// CHECK-NEXT: dealloc_stack [[TMP_OOOOA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OOOA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OOA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OB2]]#0
// CHECK-NEXT: br [[NIL_DEPTH2:bb[0-9]+]]
//   If so, drill down another level and check for Some(Some(...)).
// CHECK:    [[P]]:
// CHECK-NEXT: [[PAYLOAD_OOOA:%.*]] = unchecked_take_enum_data_addr [[TMP_OOOOA]]
// CHECK-NEXT: [[VALUE_OOOA:%.*]] = load [[PAYLOAD_OOOA]]
// CHECK-NEXT: store [[VALUE_OOOA]] to [[TMP_OOOA]]
// CHECK:      [[T1:%.*]] = select_enum_addr [[TMP_OOOA]]#1
// CHECK-NEXT: cond_br [[T1]], [[PP:bb.*]], [[NPP:bb[0-9]+]]
//   If not, finish the evaluation.
// CHECK:    [[NPP]]:
// CHECK-NEXT: dealloc_stack [[TMP_OOOOA]]#0
// CHECK-NEXT: destroy_addr [[TMP_OOOA]]#1
// CHECK-NEXT: dealloc_stack [[TMP_OOOA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OOA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OB2]]#0
// CHECK-NEXT: br [[NIL_DEPTH2]]
//   If so, drill down another level and check for Some(Some(Some(...))).
// CHECK:    [[PP]]:
// CHECK-NEXT: [[PAYLOAD_OOA:%.*]] = unchecked_take_enum_data_addr [[TMP_OOOA]]
// CHECK-NEXT: [[VALUE_OOA:%.*]] = load [[PAYLOAD_OOA]]
// CHECK-NEXT: store [[VALUE_OOA]] to [[TMP_OOA]]
// CHECK:      [[T1:%.*]] = select_enum_addr [[TMP_OOA]]#1
// CHECK-NEXT: cond_br [[T1]], [[PPP:bb.*]], [[NPPP:bb[0-9]+]]
//   If not, wrap up with Some(None).
// CHECK:    [[NPPP]]:
// CHECK-NEXT: dealloc_stack [[TMP_OOOOA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OOOA]]#0
// CHECK-NEXT: destroy_addr [[TMP_OOA]]#1
// CHECK-NEXT: dealloc_stack [[TMP_OOA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OB2]]#0
// CHECK-NEXT: br [[NIL_DEPTH1:bb[0-9]+]]
//   If so, drill down another level and check for Some(Some(Some(Some(...)))).
// CHECK:    [[PPP]]:
// CHECK-NEXT: [[PAYLOAD_OA:%.*]] = unchecked_take_enum_data_addr [[TMP_OOA]]
// CHECK-NEXT: [[VALUE_OA:%.*]] = load [[PAYLOAD_OA]]
// CHECK-NEXT: store [[VALUE_OA]] to [[TMP_OA]]
// CHECK:      [[T1:%.*]] = select_enum_addr [[TMP_OA]]#1
// CHECK-NEXT: cond_br [[T1]], [[PPPP:bb.*]], [[NPPPP:bb[0-9]+]]
//   If not, wrap up with Some(Some(None)).
// CHECK:    [[NPPPP]]:
// CHECK-NEXT: dealloc_stack [[TMP_OOOOA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OOOA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OOA]]#0
// CHECK-NEXT: destroy_addr [[TMP_OA]]#1
// CHECK-NEXT: dealloc_stack [[TMP_OA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OB2]]#0
// CHECK-NEXT: br [[NIL_DEPTH0:bb[0-9]+]]
//   If so, pull out the A and check whether it's a B.
// CHECK:    [[PPPP]]:
// CHECK-NEXT: [[TMP_A:%.*]] = unchecked_take_enum_data_addr [[TMP_OA]]
// CHECK-NEXT: [[VAL:%.*]] = load [[TMP_A]]
// CHECK-NEXT: [[TMP_OB2_VALUE:%.*]] = init_enum_data_addr [[TMP_OB2]]#1 : $*Optional<B>, #Optional.Some
// CHECK-NEXT: checked_cast_br [[VAL]] : $A to $B, [[IS_B:bb.*]], [[NOT_B:bb[0-9]+]]
//   If so, inject it back into an optional.
//   TODO: We're going to switch back out of this; we really should peephole it.
// CHECK:    [[IS_B]]([[T0:%.*]] : $B):
// CHECK-NEXT: store [[T0]] to [[TMP_OB2_VALUE]]
// CHECK-NEXT: inject_enum_addr [[TMP_OB2]]#1 : $*Optional<B>, #Optional.Some
// CHECK-NEXT: br [[SWITCH_OB2:bb[0-9]+]]
//   If not, inject nothing into an optional.
// CHECK:    [[NOT_B]]:
// CHECK-NEXT: strong_release [[VAL]]
// CHECK-NEXT: inject_enum_addr [[TMP_OB2]]#1 : $*Optional<B>, #Optional.None
// CHECK-NEXT: br [[SWITCH_OB2]]
//   Switch out on the value in [[OB2]].
// CHECK:    [[SWITCH_OB2]]:
// CHECK:      [[T1:%.*]] = select_enum_addr [[TMP_OB2]]#1
// CHECK-NEXT: cond_br [[T1]], [[IS_B2:bb.*]], [[NOT_B2:bb[0-9]+]]
//   If it's not present, finish the evaluation.
// CHECK:    [[NOT_B2]]:
// CHECK-NEXT: dealloc_stack [[TMP_OOOOA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OOOA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OOA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OA]]#0
// CHECK-NEXT: destroy_addr [[TMP_OB2]]#1
// CHECK-NEXT: dealloc_stack [[TMP_OB2]]#0
// CHECK-NEXT: br [[NIL_DEPTH2]]
//   If it's present, set OB := Some(x).
// CHECK:    [[IS_B2]]:
// CHECK-NEXT: [[PAYLOAD_B:%.*]] = unchecked_take_enum_data_addr [[TMP_OB2]]
// CHECK-NEXT: [[VALUE_B:%.*]] = load [[PAYLOAD_B]]
// CHECK-NEXT: store [[VALUE_B]] to [[TMP_B]]
// CHECK-NEXT: inject_enum_addr [[TMP_OB]]{{.*}}Some
// CHECK-NEXT: dealloc_stack [[TMP_OOOOA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OOOA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OOA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OB2]]#0
// CHECK-NEXT: br [[DONE_DEPTH0:bb[0-9]+]]
//   On various failure paths, set OB := nil.
// CHECK:    [[NIL_DEPTH0]]:
// CHECK-NEXT: inject_enum_addr [[TMP_OB]]{{.*}}None
// CHECK-NEXT: br [[DONE_DEPTH0]]
//   Set OOB := Some(OB).
// CHECK:    [[DONE_DEPTH0]]:
// CHECK-NEXT: inject_enum_addr [[TMP_OOB]]{{.*}}Some
// CHECK-NEXT: br [[DONE_DEPTH1:bb[0-9]+]]
//   On various failure paths, set OOB := nil.
// CHECK:    [[NIL_DEPTH1]]:
// CHECK-NEXT: inject_enum_addr [[TMP_OOB]]{{.*}}None
// CHECK-NEXT: br [[DONE_DEPTH1]]
//   Set X := Some(OOB).
// CHECK:    [[DONE_DEPTH1]]:
// CHECK-NEXT: inject_enum_addr [[X]]{{.*}}Some
// CHECK-NEXT: br [[DONE_DEPTH2:bb[0-9]+]]
//   On various failure paths, set X := nil.
// CHECK:    [[NIL_DEPTH2]]:
// CHECK-NEXT: inject_enum_addr [[X]]{{.*}}None
// CHECK-NEXT: br [[DONE_DEPTH2]]
//   Done.
// CHECK:    [[DONE_DEPTH2]]:
// CHECK-NEXT: strong_release [[X]]#0
// CHECK-NEXT: release_value %0

func baz(y : AnyObject?) {
  var x = (y as? B)
}
// CHECK-DAG: sil hidden @_TF4main3baz
// CHECK:      [[X:%.*]] = alloc_box $Optional<B>
// CHECK-NEXT: [[TMP_OPTANY:%.*]] = alloc_stack $Optional<AnyObject>
// CHECK-NEXT: retain_value %0
// CHECK-NEXT: store %0 to [[TMP_OPTANY]]#1
// CHECK:      [[T1:%.*]] = select_enum_addr [[TMP_OPTANY]]#1
// CHECK:      [[TMP_ANY:%.*]] = unchecked_take_enum_data_addr [[TMP_OPTANY]]
// CHECK-NEXT: [[VAL:%.*]] = load [[TMP_ANY]]
// CHECK-NEXT: [[X_VALUE:%.*]] = init_enum_data_addr [[X]]#1 : $*Optional<B>, #Optional.Some
// CHECK-NEXT: checked_cast_br [[VAL]] : $AnyObject to $B, [[IS_B:bb.*]], [[NOT_B:bb[0-9]+]]
