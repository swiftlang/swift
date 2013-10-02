// RUN: %swift -emit-silgen %s | FileCheck %s

func foo(f : (()->())?) {
  f?()
}
// CHECK:    sil @_T8optional3fooFT1fGSqFT_T___T_ : $[thin] (f: Optional<() -> ()>) -> () {
// CHECK:    bb0([[T0:%.*]] : $Optional<() -> ()>):
// CHECK-NEXT: [[F:%.*]] = alloc_box $Optional<() -> ()>
// CHECK-NEXT: store [[T0]] to [[F]]#1
//   This is for the () to ()? conversion.
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[INJECTVALUE:%.*]] = function_ref @_TSs24_injectValueIntoOptionalU__FT1vQ__GSqQ__ : $[thin] <T> (v: T) -> Optional<T>
//   Load 'f' and put the result in a temporary.
// CHECK-NEXT: [[TEMP:%.*]] = alloc_stack $Optional<() -> ()>
// CHECK-NEXT: [[T0:%.*]] = load [[F]]#1
// CHECK-NEXT: [[T1:%.*]] = copy_value [[T0]]
// CHECK-NEXT: store [[T1]] to [[TEMP]]#1
//   Check whether that temporary holds a value.
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[T0:%.*]] = function_ref @_TSs22_doesOptionalHaveValueU__FT1vRGSqQ___Bi1_ : $[thin] <T> (v: [inout] Optional<T>) -> Builtin.Int1
// CHECK-NEXT: [[T1:%.*]] = apply [[T0]]<T = () -> ()>([[TEMP]]#1)
// CHECK-NEXT: condbranch [[T1]], bb2, bb1
//   If not, leave all the cleanups we needed and jump to the nothing block.
// CHECK:    bb1:
// CHECK-NEXT: destroy_addr [[TEMP]]#1
// CHECK-NEXT: dealloc_stack [[TEMP]]#0
// CHECK-NEXT: br bb3
//   If so, pull out the value...
// CHECK:    bb2:
// CHECK-NEXT: [[T0:%.*]] = load [[TEMP]]#1
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[T1:%.*]] = function_ref @_TSs17_getOptionalValueU__FT1vGSqQ___Q_ : $[thin] <T> (v: Optional<T>) -> T
// CHECK-NEXT: [[T2:%.*]] = apply [[T1]]<T = () -> ()>([[T0]])
//   ...evaluate the rest of the suffix...
// CHECK-NEXT: [[T3:%.*]] = apply [[T2]]()
//   ...and coerce to ()?
// CHECK-NEXT: [[T4:%.*]] = apply [[INJECTVALUE]]<T = ()>()
// CHECK-NEXT: dealloc_stack [[TEMP]]#0
// CHECK-NEXT: br bb4([[T4]] : $Optional<()>)
//   Nothing block.
// CHECK:    bb3:
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[T0:%.*]] = function_ref @_TSs26_injectNothingIntoOptionalU__FT_GSqQ__ : $[thin] <T> () -> Optional<T>
// CHECK-NEXT: [[T1:%.*]] = apply [[T0]]<T = ()>()
// CHECK-NEXT: br bb4([[T1]] : $Optional<()>)
//   Continuation block.
// CHECK:    bb4([[T0:%.*]] : $Optional<()>)
// CHECK-NEXT: strong_release [[F]]#0
// CHECK-NEXT: [[T0:%.*]] = tuple ()
// CHECK-NEXT: return [[T0]] : $()

func foo<T>(f : (()->T)?) {
  var x = f?()
}
// CHECK:    sil @_T8optional3fooU__FT1fGSqFT_Q___T_ : $[thin] <T> (f: Optional<() -> T>) -> ()
// CHECK:    bb0([[T0:%.*]] : $Optional<() -> T>):
// CHECK-NEXT: [[F:%.*]] = alloc_box $Optional<() -> T>
// CHECK-NEXT: store [[T0]] to [[F]]#1
// CHECK-NEXT: [[X:%.*]] = alloc_box $Optional<T>
//   This is for the T to T? conversion.
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[INJECTVALUE:%.*]] = function_ref @_TSs24_injectValueIntoOptionalU__FT1vQ__GSqQ__ : $[thin] <T> (v: T) -> Optional<T>
//   Load 'f' and put the result in a temporary.
// CHECK-NEXT: [[TEMP:%.*]] = alloc_stack $Optional<() -> T>
// CHECK-NEXT: [[T0:%.*]] = load [[F]]#1
// CHECK-NEXT: [[T1:%.*]] = copy_value [[T0]]
// CHECK-NEXT: store [[T1]] to [[TEMP]]#1
//   Check whether that temporary holds a value.
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[T0:%.*]] = function_ref @_TSs22_doesOptionalHaveValueU__FT1vRGSqQ___Bi1_ : $[thin] <T> (v: [inout] Optional<T>) -> Builtin.Int1
// CHECK-NEXT: [[T1:%.*]] = apply [[T0]]<T = () -> T>([[TEMP]]#1)
// CHECK-NEXT: condbranch [[T1]], bb2, bb1
//   If not, leave all the cleanups we needed and jump to the nothing block.
// CHECK:    bb1:
// CHECK-NEXT: destroy_addr [[TEMP]]#1
// CHECK-NEXT: dealloc_stack [[TEMP]]#0
// CHECK-NEXT: br bb3
//   If so, pull out the value...
// CHECK:    bb2:
// CHECK-NEXT: [[T0:%.*]] = load [[TEMP]]#1
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[T1:%.*]] = function_ref @_TSs17_getOptionalValueU__FT1vGSqQ___Q_ : $[thin] <T> (v: Optional<T>) -> T
// CHECK-NEXT: [[T2:%.*]] = apply [[T1]]<T = () -> T>([[T0]])
//   ...evaluate the rest of the suffix...
// CHECK-NEXT: [[TEMP2:%.*]] = alloc_stack $T
// CHECK-NEXT: apply [[T2]]([[TEMP2]]#1)
//   ...and coerce to ()?
// CHECK-NEXT: apply [[INJECTVALUE]]<T = T>([[X]]#1, [[TEMP2]]#1)
// CHECK-NEXT: dealloc_stack [[TEMP2]]#0
// CHECK-NEXT: dealloc_stack [[TEMP]]#0
// CHECK-NEXT: br bb4
//   Nothing block.
// CHECK:    bb3:
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[T0:%.*]] = function_ref @_TSs26_injectNothingIntoOptionalU__FT_GSqQ__ : $[thin] <T> () -> Optional<T>
// CHECK-NEXT: apply [[T0]]<T = T>([[X]]#1)
// CHECK-NEXT: br bb4
//   Continuation block.
// CHECK:    bb4
// CHECK-NEXT: strong_release [[X]]#0
// CHECK-NEXT: strong_release [[F]]#0
// CHECK-NEXT: [[T0:%.*]] = tuple ()
// CHECK-NEXT: return [[T0]] : $()
