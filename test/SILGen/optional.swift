// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

func foo(var f: (()->())?) {
  f?()
}
// CHECK:    sil hidden @{{.*}}foo{{.*}} : $@thin (@owned Optional<() -> ()>) -> () {
// CHECK:    bb0([[T0:%.*]] : $Optional<() -> ()>):
// CHECK-NEXT: [[F:%.*]] = alloc_box $Optional<() -> ()>
// CHECK-NEXT: store [[T0]] to [[F]]#1
// CHECK-NEXT: [[RESULT:%.*]] = alloc_stack $Optional<()>
// CHECK-NEXT: [[TEMP_RESULT:%.*]] = init_enum_data_addr [[RESULT]]
//   Switch out on the lvalue (() -> ())!:
// CHECK:      [[T1:%.*]] = select_enum_addr [[F]]#1
// CHECK-NEXT: cond_br [[T1]], bb2, bb1
//   If it doesn't have a value, kill all the temporaries and jump to
//   the first nothing block.
// CHECK:    bb1:
// CHECK-NEXT: br bb3
//   If it does, project and load the value out of the implicitly unwrapped
//   optional...
// CHECK:    bb2:
// CHECK-NEXT: [[FN0_ADDR:%.*]] = unchecked_take_enum_data_addr [[F]]
// CHECK-NEXT: [[FN0:%.*]] = load [[FN0_ADDR]]
//   ...unnecessarily reabstract back to () -> ()...
// CHECK:      [[T0:%.*]] = function_ref @_TTRXFo_iT__iT__XFo__dT__ : $@thin (@owned @callee_owned (@out (), @in ()) -> ()) -> ()
// CHECK-NEXT: [[FN1:%.*]] = partial_apply [[T0]]([[FN0]])
//   .... then call it
// CHECK-NEXT: apply [[FN1]]()
// CHECK:      br bb4
//   (first nothing block)
// CHECK:    bb3:
// CHECK-NEXT: inject_enum_addr [[RESULT]]
// CHECK-NEXT: br bb4

func foo2<T>(var f: (()->T)?) {
  var x = f?()
}
// CHECK-LABEL: sil hidden @{{.*}}foo{{.*}} : $@thin <T> (@owned Optional<() -> T>) -> ()
// CHECK:    bb0([[T0:%.*]] : $Optional<() -> T>):
// CHECK-NEXT: [[F:%.*]] = alloc_box $Optional<() -> T>
// CHECK-NEXT: store [[T0]] to [[F]]#1
// CHECK-NEXT: [[X:%.*]] = alloc_box $Optional<T>
// CHECK-NEXT: [[TEMP:%.*]] = init_enum_data_addr [[X]]
//   Check whether 'f' holds a value.
// CHECK:      [[T1:%.*]] = select_enum_addr [[F]]#1
// CHECK-NEXT: cond_br [[T1]], bb2, bb1
//   If not, leave all the cleanups we needed and jump to the nothing block.
// CHECK:    bb1:
// CHECK-NEXT: br bb3
//   If so, pull out the value...
// CHECK:    bb2:
// CHECK-NEXT: [[T1:%.*]] = unchecked_take_enum_data_addr [[F]]#1
// CHECK-NEXT: [[T0:%.*]] = load [[T1]]
// CHECK-NEXT: strong_retain
//   ...evaluate the rest of the suffix...
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[THUNK:%.*]] = function_ref @{{.*}} : $@thin <τ_0_0> (@out τ_0_0, @owned @callee_owned (@out τ_0_0, @in ()) -> ()) -> ()
// CHECK-NEXT: [[T1:%.*]] = partial_apply [[THUNK]]<T>([[T0]])
// CHECK-NEXT: apply [[T1]]([[TEMP_RESULT]])
//   ...and coerce to T?
// CHECK-NEXT: inject_enum_addr [[X]]{{.*}}Some
// CHECK-NEXT: br bb4
//   Nothing block.
// CHECK:    bb3:
// CHECK-NEXT: inject_enum_addr [[X]]{{.*}}None
// CHECK-NEXT: br bb4
//   Continuation block.
// CHECK:    bb4
// CHECK-NEXT: strong_release [[X]]#0
// CHECK-NEXT: strong_release [[F]]#0
// CHECK-NEXT: [[T0:%.*]] = tuple ()
// CHECK-NEXT: return [[T0]] : $()

// <rdar://problem/15180622>

func wrap<T>(x: T) -> T? { return x }

// CHECK: sil hidden @_TF8optional16wrap_then_unwrap
func wrap_then_unwrap<T>(x: T) -> T {
  // CHECK: [[FORCE:%.*]] = function_ref @_TFSs17_getOptionalValueU__FGSqQ__Q_
  // CHECK: apply [transparent] [[FORCE]]<{{.*}}>(%0, {{%.*}})
  return wrap(x)!
}

// CHECK: sil hidden @_TF8optional10tuple_bind
func tuple_bind(x: (Int, String)?) -> String? {
  return x?.1
  // CHECK:   cond_br {{%.*}}, [[NONNULL:bb[0-9]+]], [[NULL:bb[0-9]+]]
  // CHECK: [[NONNULL]]:
  // CHECK:   [[STRING:%.*]] = tuple_extract {{%.*}} : $(Int, String), 1
  // CHECK-NOT: release_value [[STRING]]
}
