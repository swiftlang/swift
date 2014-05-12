// RUN: %swift -emit-silgen %s | FileCheck %s

func foo(var f: (()->())?) {
  f?()
}
// CHECK:    sil @{{.*}}foo{{.*}} : $@thin (@owned Optional<() -> ()>) -> () {
// CHECK:    bb0([[T0:%.*]] : $Optional<() -> ()>):
// CHECK-NEXT: [[F:%.*]] = alloc_box $Optional<() -> ()>
// CHECK-NEXT: store [[T0]] to [[F]]#1
// CHECK-NEXT: [[RESULT:%.*]] = alloc_stack $Optional<()>
// CHECK-NEXT: [[TEMP_RESULT:%.*]] = alloc_stack $()
//   Copy 'f' into a temporary.
// CHECK-NEXT: [[TEMP_OPTFN:%.*]] = alloc_stack $Optional<() -> ()>
// CHECK-NEXT: copy_addr [[F]]#1 to [initialization] [[TEMP_OPTFN]]#1
//   Check whether that temporary holds a value.
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[T0:%.*]] = function_ref @_TFSs22_doesOptionalHaveValueU__FRGSqQ__Bi1_ : $@thin <τ_0_0> (@inout Optional<τ_0_0>) -> Builtin.Int1
// CHECK-NEXT: [[T1:%.*]] = apply [transparent] [[T0]]<() -> ()>([[TEMP_OPTFN]]#1)
// CHECK-NEXT: cond_br [[T1]], bb2, bb1
//   If not, leave all the cleanups we needed and jump to the nothing block.
// CHECK:    bb1:
// CHECK-NEXT: destroy_addr [[TEMP_OPTFN]]#1
// CHECK-NEXT: dealloc_stack [[TEMP_OPTFN]]#0
// CHECK-NEXT: dealloc_stack [[TEMP_RESULT]]#0
// CHECK-NEXT: br bb3
//   If so, pull out the value...
// CHECK:    bb2:
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[T0:%.*]] = function_ref @_TFSs17_getOptionalValueU__FGSqQ__Q_ : $@thin <τ_0_0> (@out τ_0_0, @in Optional<τ_0_0>) -> ()
// CHECK-NEXT: [[TEMP_FN:%.*]] = alloc_stack $@callee_owned (@out (), @in ()) -> ()
// CHECK-NEXT: apply [transparent] [[T0]]<() -> ()>([[TEMP_FN]]#1, [[TEMP_OPTFN]]#1)
// CHECK-NEXT: [[T0:%.*]] = load [[TEMP_FN]]#1
//   ...evaluate the rest of the suffix...
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[T1:%.*]] = function_ref @{{.*}} : $@thin (@owned @callee_owned (@out (), @in ()) -> ()) -> ()
// CHECK-NEXT: [[T2:%.*]] = partial_apply [[T1]]([[T0]])
// CHECK-NEXT: [[T3:%.*]] = apply [[T2]]()
//   ...and coerce to ()?
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[T0:%.*]] = function_ref @_TFSs24_injectValueIntoOptionalU__FQ_GSqQ__ : $@thin <τ_0_0> (@out Optional<τ_0_0>, @in τ_0_0) -> ()
// CHECK-NEXT: [[T1:%.*]] = apply [transparent] [[T0]]<()>([[RESULT]]#1, [[TEMP_RESULT]]#1)
// CHECK-NEXT: dealloc_stack [[TEMP_FN]]#0
// CHECK-NEXT: dealloc_stack [[TEMP_OPTFN]]#0
// CHECK-NEXT: dealloc_stack [[TEMP_RESULT]]#0
// CHECK-NEXT: br bb4
//   Nothing block.
// CHECK:    bb3:
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[T0:%.*]] = function_ref @_TFSs26_injectNothingIntoOptionalU__FT_GSqQ__ : $@thin <τ_0_0> (@out Optional<τ_0_0>) -> ()
// CHECK-NEXT: apply [transparent] [[T0]]<()>([[RESULT]]#1)
// CHECK-NEXT: br bb4
//   Continuation block.
// CHECK:    bb4:
// CHECK-NEXT: [[T0:%.*]] = load [[RESULT]]#1
// CHECK-NEXT: dealloc_stack [[RESULT]]#0
// CHECK-NEXT: strong_release [[F]]#0
// CHECK-NEXT: [[T0:%.*]] = tuple ()
// CHECK-NEXT: return [[T0]] : $()

func foo2<T>(var f: (()->T)?) {
  var x = f?()
}
// CHECK-LABEL: sil @{{.*}}foo{{.*}} : $@thin <T> (@owned Optional<() -> T>) -> ()
// CHECK:    bb0([[T0:%.*]] : $Optional<() -> T>):
// CHECK-NEXT: [[F:%.*]] = alloc_box $Optional<() -> T>
// CHECK-NEXT: store [[T0]] to [[F]]#1
// CHECK-NEXT: [[X:%.*]] = alloc_box $Optional<T>
//   Copy 'f' into a temporary.
// CHECK-NEXT: [[TEMP_RESULT:%.*]] = alloc_stack $T
// CHECK-NEXT: [[TEMP_OPTFN:%.*]] = alloc_stack $Optional<() -> T>
// CHECK-NEXT: copy_addr [[F]]#1 to [initialization] [[TEMP_OPTFN]]#1
//   Check whether that temporary holds a value.
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[T0:%.*]] = function_ref @_TFSs22_doesOptionalHaveValueU__FRGSqQ__Bi1_ : $@thin <τ_0_0> (@inout Optional<τ_0_0>) -> Builtin.Int1
// CHECK-NEXT: [[T1:%.*]] = apply [transparent] [[T0]]<() -> T>([[TEMP_OPTFN]]#1)
// CHECK-NEXT: cond_br [[T1]], bb2, bb1
//   If not, leave all the cleanups we needed and jump to the nothing block.
// CHECK:    bb1:
// CHECK-NEXT: destroy_addr [[TEMP_OPTFN]]#1
// CHECK-NEXT: dealloc_stack [[TEMP_OPTFN]]#0
// CHECK-NEXT: dealloc_stack [[TEMP_RESULT]]#0
// CHECK-NEXT: br bb3
//   If so, pull out the value...
// CHECK:    bb2:
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[T0:%.*]] = function_ref @_TFSs17_getOptionalValueU__FGSqQ__Q_ : $@thin <τ_0_0> (@out τ_0_0, @in Optional<τ_0_0>) -> ()
// CHECK-NEXT: [[TEMP_FN:%.*]] = alloc_stack $@callee_owned (@out T, @in ()) -> ()
// CHECK-NEXT: apply [transparent] [[T0]]<() -> T>([[TEMP_FN]]#1, [[TEMP_OPTFN]]#1)
// CHECK-NEXT: [[T0:%.*]] = load [[TEMP_FN]]#1
//   ...evaluate the rest of the suffix...
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[THUNK:%.*]] = function_ref @{{.*}} : $@thin <τ_0_0> (@out τ_0_0, @owned @callee_owned (@out τ_0_0, @in ()) -> ()) -> ()
// CHECK-NEXT: [[T1:%.*]] = partial_apply [[THUNK]]<T>([[T0]])
// CHECK-NEXT: apply [[T1]]([[TEMP_RESULT]]#1)
//   ...and coerce to T?
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[T0:%.*]] = function_ref @_TFSs24_injectValueIntoOptionalU__FQ_GSqQ__ : $@thin <τ_0_0> (@out Optional<τ_0_0>, @in τ_0_0) -> ()
// CHECK-NEXT: apply [transparent] [[T0]]<T>([[X]]#1, [[TEMP_RESULT]]#1)
// CHECK-NEXT: dealloc_stack [[TEMP_FN]]#0
// CHECK-NEXT: dealloc_stack [[TEMP_OPTFN]]#0
// CHECK-NEXT: dealloc_stack [[TEMP_RESULT]]#0
// CHECK-NEXT: br bb4
//   Nothing block.
// CHECK:    bb3:
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[T0:%.*]] = function_ref @_TFSs26_injectNothingIntoOptionalU__FT_GSqQ__ : $@thin <τ_0_0> (@out Optional<τ_0_0>) -> ()
// CHECK-NEXT: apply [transparent] [[T0]]<T>([[X]]#1)
// CHECK-NEXT: br bb4
//   Continuation block.
// CHECK:    bb4
// CHECK-NEXT: strong_release [[X]]#0
// CHECK-NEXT: strong_release [[F]]#0
// CHECK-NEXT: [[T0:%.*]] = tuple ()
// CHECK-NEXT: return [[T0]] : $()

// <rdar://problem/15180622>

func wrap<T>(x: T) -> T? { return x }

// CHECK: sil @_TF8optional16wrap_then_unwrap
func wrap_then_unwrap<T>(x: T) -> T {
  // CHECK: [[FORCE:%.*]] = function_ref @_TFSs17_getOptionalValueU__FGSqQ__Q_
  // CHECK: apply [transparent] [[FORCE]]<{{.*}}>(%0, {{%.*}})
  return wrap(x)!
}

// CHECK: sil @_TF8optional10tuple_bind
func tuple_bind(x: (Int, String)?) -> String? {
  return x?.1
  // CHECK:   cond_br {{%.*}}, [[NONNULL:bb[0-9]+]], [[NULL:bb[0-9]+]]
  // CHECK: [[NONNULL]]:
  // CHECK:   [[STRING:%.*]] = tuple_extract {{%.*}} : $(Int, String), 1
  // CHECK-NOT: release_value [[STRING]]
}
