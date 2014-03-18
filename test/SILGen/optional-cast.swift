// RUN: %swift -emit-silgen %s | FileCheck %s

class A {}
class B : A {}

func foo(y : A?) {
  var x = (y as B)
}
// CHECK-DAG: sil @_TF4main3fooFT1yGSqCS_1A__T_ : $@thin (@owned Optional<A>) -> () {
// CHECK:      [[X:%.*]] = alloc_box $Optional<B>
//   Materialize the parameter.
// CHECK-NEXT: [[TMP_OPTA:%.*]] = alloc_stack $Optional<A>
// CHECK-NEXT: [[T1:%.*]] = copy_value %0
// CHECK-NEXT: store [[T1]] to [[TMP_OPTA]]#1
//   Check whether the temporary holds a value.
// CHECK:      [[T0:%.*]] = function_ref @_TFSs22_doesOptionalHaveValueU__FT1vRGSqQ___Bi1_
// CHECK-NEXT: [[T1:%.*]] = apply [transparent] [[T0]]<A>([[TMP_OPTA]]#1)
// CHECK-NEXT: cond_br [[T1]], [[IS_PRESENT:bb.*]], [[NOT_PRESENT:bb[0-9]+]]
//   If not, destroy the temporary.
// CHECK:    [[NOT_PRESENT]]:
// CHECK-NEXT: destroy_addr [[TMP_OPTA]]#1
// CHECK-NEXT: dealloc_stack [[TMP_OPTA]]#0
// CHECK-NEXT: br [[NOT_PRESENT:bb[0-9]+]]
//   If so, pull the value out and check whether it's a B.
// CHECK:    [[IS_PRESENT]]:
// CHECK:      [[T0:%.*]] = function_ref @_TFSs17_getOptionalValueU__FT1vGSqQ___Q_
// CHECK-NEXT: [[TMP_A:%.*]] = alloc_stack $A
// CHECK-NEXT: apply [transparent] [[T0]]<A>([[TMP_A]]#1, [[TMP_OPTA]]#1)
// CHECK-NEXT: [[VAL:%.*]] = load [[TMP_A]]#1
// CHECK-NEXT: checked_cast_br downcast [[VAL]] : $A to $B, [[IS_B:bb.*]], [[NOT_B:bb[0-9]+]]
//   If so, materialize that and inject it into x.
// CHECK:    [[IS_B]]([[T0:%.*]] : $B):
// CHECK-NEXT: [[TMP_B:%.*]] = alloc_stack $B
// CHECK-NEXT: store [[T0]] to [[TMP_B]]
// CHECK:      [[T0:%.*]] = function_ref @_TFSs24_injectValueIntoOptionalU__FT1vQ__GSqQ__
// CHECK-NEXT: apply [transparent] [[T0]]<B>([[X]]#1, [[TMP_B]]#1)
// CHECK-NEXT: dealloc_stack [[TMP_B]]#0
// CHECK-NEXT: br [[CONT:bb[0-9]+]]
//   If not, release the A and inject nothing into x.
// CHECK:    [[NOT_B]]:
// CHECK-NEXT: strong_release [[VAL]]
// CHECK:      [[T0:%.*]] = function_ref @_TFSs26_injectNothingIntoOptionalU__FT_GSqQ__
// CHECK-NEXT: apply [transparent] [[T0]]<B>([[X]]#1)
// CHECK-NEXT: br [[CONT]]
//   Finish the present path.
// CHECK:    [[CONT]]:
// CHECK-NEXT: dealloc_stack [[TMP_A]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OPTA]]#0
// CHECK-NEXT: br [[CONT:bb[0-9]+]]
//   Finish the not-present path.
// CHECK:    [[NOT_PRESENT]]:
// CHECK:      [[T0:%.*]] = function_ref @_TFSs26_injectNothingIntoOptionalU__FT_GSqQ__
// CHECK-NEXT: apply [transparent] [[T0]]<B>([[X]]#1)
// CHECK-NEXT: br [[CONT]]
//   Finish.
// CHECK:    [[CONT]]:
// CHECK-NEXT: strong_release [[X]]#0
// CHECK-NEXT: destroy_value %0

func bar(y : A????) {
  var x = (y as B??)
}
// CHECK-DAG: sil @_TF4main3barFT1yGSqGSqGSqGSqCS_1A_____T_ : $@thin (@owned Optional<Optional<Optional<Optional<A>>>>) -> ()
// CHECK:      [[X:%.*]] = alloc_box $Optional<Optional<Optional<B>>>
// CHECK-NEXT: [[TMP_OOB:%.*]] = alloc_stack $Optional<Optional<B>>
// CHECK-NEXT: [[TMP_OB:%.*]] = alloc_stack $Optional<B>
// CHECK-NEXT: [[TMP_B:%.*]] = alloc_stack $B
// CHECK-NEXT: [[TMP_OB2:%.*]] = alloc_stack $Optional<B>
// CHECK-NEXT: [[TMP_OA:%.*]] = alloc_stack $Optional<A>
// CHECK-NEXT: [[TMP_OOA:%.*]] = alloc_stack $Optional<Optional<A>>
// CHECK-NEXT: [[TMP_OOOA:%.*]] = alloc_stack $Optional<Optional<Optional<A>>>
// CHECK-NEXT: [[TMP_OOOOA:%.*]] = alloc_stack $Optional<Optional<Optional<Optional<A>>>>
//   Materialize the argument and check for Some(...)
// CHECK-NEXT: [[T0:%.*]] = copy_value %0
// CHECK-NEXT: store [[T0]] to [[TMP_OOOOA]]#1
// CHECK:      [[T0:%.*]] = function_ref @_TFSs22_doesOptionalHaveValueU__FT1vRGSqQ___Bi1_
// CHECK-NEXT: [[T1:%.*]] = apply [transparent] [[T0]]<Optional<Optional<Optional<A>>>>([[TMP_OOOOA]]#1)
// CHECK-NEXT: cond_br [[T1]], [[P:bb.*]], [[NP:bb[0-9]+]]
//   If not, finish the evaluation.
// CHECK:    [[NP]]:
// CHECK-NEXT: destroy_addr [[TMP_OOOOA]]#1
// CHECK-NEXT: dealloc_stack [[TMP_OOOOA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OOOA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OOA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OB2]]#0
// CHECK-NEXT: dealloc_stack [[TMP_B]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OB]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OOB]]#0
// CHECK-NEXT: br [[NIL_DEPTH2:bb[0-9]+]]
//   If so, drill down another level and check for Some(Some(...)).
// CHECK:    [[P]]:
// CHECK:      [[T0:%.*]] = function_ref @_TFSs17_getOptionalValueU__FT1vGSqQ___Q_
// CHECK-NEXT: apply [transparent] [[T0]]<Optional<Optional<Optional<A>>>>([[TMP_OOOA]]#1, [[TMP_OOOOA]]#1)
// CHECK:      [[T0:%.*]] = function_ref @_TFSs22_doesOptionalHaveValueU__FT1vRGSqQ___Bi1_
// CHECK-NEXT: [[T1:%.*]] = apply [transparent] [[T0]]<Optional<Optional<A>>>([[TMP_OOOA]]#1)
// CHECK-NEXT: cond_br [[T1]], [[PP:bb.*]], [[NPP:bb[0-9]+]]
//   If not, finish the evaluation.
// CHECK:    [[NPP]]:
// CHECK-NEXT: dealloc_stack [[TMP_OOOOA]]#0
// CHECK-NEXT: destroy_addr [[TMP_OOOA]]#1
// CHECK-NEXT: dealloc_stack [[TMP_OOOA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OOA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OB2]]#0
// CHECK-NEXT: dealloc_stack [[TMP_B]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OB]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OOB]]#0
// CHECK-NEXT: br [[NIL_DEPTH2]]
//   If so, drill down another level and check for Some(Some(Some(...))).
// CHECK:    [[PP]]:
// CHECK:      [[T0:%.*]] = function_ref @_TFSs17_getOptionalValueU__FT1vGSqQ___Q_
// CHECK-NEXT: apply [transparent] [[T0]]<Optional<Optional<A>>>([[TMP_OOA]]#1, [[TMP_OOOA]]#1)
// CHECK:      [[T0:%.*]] = function_ref @_TFSs22_doesOptionalHaveValueU__FT1vRGSqQ___Bi1_
// CHECK-NEXT: [[T1:%.*]] = apply [transparent] [[T0]]<Optional<A>>([[TMP_OOA]]#1)
// CHECK-NEXT: cond_br [[T1]], [[PPP:bb.*]], [[NPPP:bb[0-9]+]]
//   If not, wrap up with Some(None).
// CHECK:    [[NPPP]]:
// CHECK-NEXT: dealloc_stack [[TMP_OOOOA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OOOA]]#0
// CHECK-NEXT: destroy_addr [[TMP_OOA]]#1
// CHECK-NEXT: dealloc_stack [[TMP_OOA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OB2]]#0
// CHECK-NEXT: dealloc_stack [[TMP_B]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OB]]#0
// CHECK-NEXT: br [[NIL_DEPTH1:bb[0-9]+]]
//   If so, drill down another level and check for Some(Some(Some(Some(...)))).
// CHECK:    [[PPP]]:
// CHECK:      [[T0:%.*]] = function_ref @_TFSs17_getOptionalValueU__FT1vGSqQ___Q_
// CHECK-NEXT: apply [transparent] [[T0]]<Optional<A>>([[TMP_OA]]#1, [[TMP_OOA]]#1)
// CHECK:      [[T0:%.*]] = function_ref @_TFSs22_doesOptionalHaveValueU__FT1vRGSqQ___Bi1_
// CHECK-NEXT: [[T1:%.*]] = apply [transparent] [[T0]]<A>([[TMP_OA]]#1)
// CHECK-NEXT: cond_br [[T1]], [[PPPP:bb.*]], [[NPPPP:bb[0-9]+]]
//   If not, wrap up with Some(Some(None)).
// CHECK:    [[NPPPP]]:
// CHECK-NEXT: dealloc_stack [[TMP_OOOOA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OOOA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OOA]]#0
// CHECK-NEXT: destroy_addr [[TMP_OA]]#1
// CHECK-NEXT: dealloc_stack [[TMP_OA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OB2]]#0
// CHECK-NEXT: dealloc_stack [[TMP_B]]#0
// CHECK-NEXT: br [[NIL_DEPTH0:bb[0-9]+]]
//   If so, pull out the A and check whether it's a B.
// CHECK:    [[PPPP]]:
// CHECK:      [[T0:%.*]] = function_ref @_TFSs17_getOptionalValueU__FT1vGSqQ___Q_
// CHECK-NEXT: [[TMP_A:%.*]] = alloc_stack $A
// CHECK-NEXT: apply [transparent] [[T0]]<A>([[TMP_A]]#1, [[TMP_OA]]#1)
// CHECK-NEXT: [[VAL:%.*]] = load [[TMP_A]]#1
// CHECK-NEXT: checked_cast_br downcast [[VAL]] : $A to $B, [[IS_B:bb.*]], [[NOT_B:bb[0-9]+]]
//   If so, inject it back into an optional.
//   TODO: We're going to switch back out of this; we really should peephole it.
// CHECK:    [[IS_B]]([[T0:%.*]] : $B):
// CHECK-NEXT: [[TMP_B2:%.*]] = alloc_stack $B
// CHECK-NEXT: store [[T0]] to [[TMP_B2]]#1
// CHECK:      [[T0:%.*]] = function_ref @_TFSs24_injectValueIntoOptionalU__FT1vQ__GSqQ__
// CHECK-NEXT: apply [transparent] [[T0]]<B>([[TMP_OB2]]#1, [[TMP_B2]]#1)
// CHECK-NEXT: dealloc_stack [[TMP_B2]]#0
// CHECK-NEXT: br [[SWITCH_OB2:bb[0-9]+]]
//   If not, inject nothing into an optional.
// CHECK:    [[NOT_B]]:
// CHECK-NEXT: strong_release [[VAL]]
// CHECK:      [[T0:%.*]] = function_ref @_TFSs26_injectNothingIntoOptionalU__FT_GSqQ__
// CHECK-NEXT: apply [transparent] [[T0]]<B>([[TMP_OB2]]#1)
// CHECK-NEXT: br [[SWITCH_OB2]]
//   Switch out on the value in [[OB2]].
// CHECK:    [[SWITCH_OB2]]:
// CHECK:      [[T0:%.*]] = function_ref @_TFSs22_doesOptionalHaveValueU__FT1vRGSqQ___Bi1_
// CHECK-NEXT: [[T1:%.*]] = apply [transparent] [[T0]]<B>([[TMP_OB2]]#1)
// CHECK-NEXT: cond_br [[T1]], [[IS_B2:bb.*]], [[NOT_B2:bb[0-9]+]]
//   If it's not present, finish the evaluation.
// CHECK:    [[NOT_B2]]:
// CHECK-NEXT: dealloc_stack [[TMP_A]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OOOOA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OOOA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OOA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OA]]#0
// CHECK-NEXT: destroy_addr [[TMP_OB2]]#1
// CHECK-NEXT: dealloc_stack [[TMP_OB2]]#0
// CHECK-NEXT: dealloc_stack [[TMP_B]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OB]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OOB]]#0
// CHECK-NEXT: br [[NIL_DEPTH2]]
//   If it's present, set OB := Some(x).
// CHECK:    [[IS_B2]]:
// CHECK:      [[T0:%.*]] = function_ref @_TFSs17_getOptionalValueU__FT1vGSqQ___Q_
// CHECK-NEXT: apply [transparent] [[T0]]<B>([[TMP_B]]#1, [[TMP_OB2]]#1)
// CHECK:      [[T0:%.*]] = function_ref @_TFSs24_injectValueIntoOptionalU__FT1vQ__GSqQ__
// CHECK-NEXT: apply [transparent] [[T0]]<B>([[TMP_OB]]#1, [[TMP_B]]#1)
// CHECK-NEXT: dealloc_stack [[TMP_A]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OOOOA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OOOA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OOA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OA]]#0
// CHECK-NEXT: dealloc_stack [[TMP_OB2]]#0
// CHECK-NEXT: dealloc_stack [[TMP_B]]#0
// CHECK-NEXT: br [[DONE_DEPTH0:bb[0-9]+]]
//   On various failure paths, set OB := nil.
// CHECK:    [[NIL_DEPTH0]]:
// CHECK:      [[T0:%.*]] = function_ref @_TFSs26_injectNothingIntoOptionalU__FT_GSqQ__
// CHECK-NEXT: apply [transparent] [[T0]]<B>([[TMP_OB]]#1)
// CHECK-NEXT: br [[DONE_DEPTH0]]
//   Set OOB := Some(OB).
// CHECK:    [[DONE_DEPTH0]]:
// CHECK:      [[T0:%.*]] = function_ref @_TFSs24_injectValueIntoOptionalU__FT1vQ__GSqQ__
// CHECK-NEXT: apply [transparent] [[T0]]<Optional<B>>([[TMP_OOB]]#1, [[TMP_OB]]#1)
// CHECK-NEXT: dealloc_stack [[TMP_OB]]#0
// CHECK-NEXT: br [[DONE_DEPTH1:bb[0-9]+]]
//   On various failure paths, set OOB := nil.
// CHECK:    [[NIL_DEPTH1]]:
// CHECK:      [[T0:%.*]] = function_ref @_TFSs26_injectNothingIntoOptionalU__FT_GSqQ__
// CHECK-NEXT: apply [transparent] [[T0]]<Optional<B>>([[TMP_OOB]]#1)
// CHECK-NEXT: br [[DONE_DEPTH1]]
//   Set X := Some(OOB).
// CHECK:    [[DONE_DEPTH1]]:
// CHECK:      [[T0:%.*]] = function_ref @_TFSs24_injectValueIntoOptionalU__FT1vQ__GSqQ__
// CHECK-NEXT: apply [transparent] [[T0]]<Optional<Optional<B>>>([[X]]#1, [[TMP_OOB]]#1)
// CHECK-NEXT: dealloc_stack [[TMP_OOB]]#0
// CHECK-NEXT: br [[DONE_DEPTH2:bb[0-9]+]]
//   On various failure paths, set X := nil.
// CHECK:    [[NIL_DEPTH2]]:
// CHECK:      [[T0:%.*]] = function_ref @_TFSs26_injectNothingIntoOptionalU__FT_GSqQ__
// CHECK-NEXT: apply [transparent] [[T0]]<Optional<Optional<B>>>([[X]]#1)
// CHECK-NEXT: br [[DONE_DEPTH2]]
//   Done.
// CHECK:    [[DONE_DEPTH2]]:
// CHECK-NEXT: strong_release [[X]]#0
// CHECK-NEXT: destroy_value %0
