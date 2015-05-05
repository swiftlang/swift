// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

func testCall(f: (()->())?) {
  f?()
}
// CHECK:    sil hidden @{{.*}}testCall{{.*}}
// CHECK:    bb0([[T0:%.*]] : $Optional<() -> ()>):
// CHECK:      [[T1:%.*]] = select_enum %0
// CHECK-NEXT: cond_br [[T1]], bb1, bb3
//   If it does, project and load the value out of the implicitly unwrapped
//   optional...

// CHECK: bb1:
// CHECK-NEXT: [[FN0:%.*]] = unchecked_enum_data %0 : $Optional<() -> ()>, #Optional.Some!enumelt.1
//   ...unnecessarily reabstract back to () -> ()...
// CHECK:      [[T0:%.*]] = function_ref @_TTRXFo_iT__iT__XFo__dT__ : $@convention(thin) (@owned @callee_owned (@out (), @in ()) -> ()) -> ()
// CHECK-NEXT: [[FN1:%.*]] = partial_apply [[T0]]([[FN0]])
//   .... then call it
// CHECK-NEXT: apply [[FN1]]()
// CHECK:      br bb2(
//   (first nothing block)
// CHECK:    bb3:
// CHECK-NEXT: enum $Optional<()>, #Optional.None!enumelt
// CHECK-NEXT: br bb2

func testAddrOnlyCallResult<T>(var f: (()->T)?) {
  var x = f?()
}
// CHECK-LABEL: sil hidden @{{.*}}testAddrOnlyCallResult{{.*}} : $@convention(thin) <T> (@owned Optional<() -> T>) -> ()
// CHECK:    bb0([[T0:%.*]] : $Optional<() -> T>):
// CHECK-NEXT: [[F:%.*]] = alloc_box $Optional<() -> T> // var f
// CHECK-NEXT: store [[T0]] to [[F]]#1
// CHECK-NEXT: [[X:%.*]] = alloc_box $Optional<T>  // var x
// CHECK-NEXT: [[TEMP:%.*]] = init_enum_data_addr [[X]]
//   Check whether 'f' holds a value.
// CHECK:      [[T1:%.*]] = select_enum_addr [[F]]#1
// CHECK-NEXT: cond_br [[T1]], bb1, bb3
//   If so, pull out the value...
// CHECK:    bb1:
// CHECK-NEXT: [[T1:%.*]] = unchecked_take_enum_data_addr [[F]]#1
// CHECK-NEXT: [[T0:%.*]] = load [[T1]]
// CHECK-NEXT: strong_retain
//   ...evaluate the rest of the suffix...
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[THUNK:%.*]] = function_ref @{{.*}} : $@convention(thin) <τ_0_0> (@out τ_0_0, @owned @callee_owned (@out τ_0_0, @in ()) -> ()) -> ()
// CHECK-NEXT: [[T1:%.*]] = partial_apply [[THUNK]]<T>([[T0]])
// CHECK-NEXT: apply [[T1]]([[TEMP]])
//   ...and coerce to T?
// CHECK-NEXT: inject_enum_addr [[X]]{{.*}}Some
// CHECK-NEXT: br bb2
//   Continuation block.
// CHECK:    bb2
// CHECK-NEXT: strong_release [[X]]#0
// CHECK-NEXT: strong_release [[F]]#0
// CHECK-NEXT: [[T0:%.*]] = tuple ()
// CHECK-NEXT: return [[T0]] : $()

//   Nothing block.
// CHECK:    bb3:
// CHECK-NEXT: inject_enum_addr [[X]]{{.*}}None
// CHECK-NEXT: br bb2


// <rdar://problem/15180622>

func wrap<T>(x: T) -> T? { return x }

// CHECK-LABEL: sil hidden @_TF8optional16wrap_then_unwrap
func wrap_then_unwrap<T>(x: T) -> T {
  // CHECK: [[FORCE:%.*]] = function_ref @_TFSs17_getOptionalValueU__FGSqQ__Q_
  // CHECK: apply [[FORCE]]<{{.*}}>(%0, {{%.*}})
  return wrap(x)!
}

// CHECK-LABEL: sil hidden @_TF8optional10tuple_bind
func tuple_bind(x: (Int, String)?) -> String? {
  return x?.1
  // CHECK:   cond_br {{%.*}}, [[NONNULL:bb[0-9]+]], [[NULL:bb[0-9]+]]
  // CHECK: [[NONNULL]]:
  // CHECK:   [[STRING:%.*]] = tuple_extract {{%.*}} : $(Int, String), 1
  // CHECK-NOT: release_value [[STRING]]
}


