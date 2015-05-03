// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

func foo(var f f: (()->())!) {
  f?()
}
// CHECK:    sil hidden @{{.*}}foo{{.*}} : $@convention(thin) (@owned ImplicitlyUnwrappedOptional<() -> ()>) -> () {
// CHECK:    bb0([[T0:%.*]] : $ImplicitlyUnwrappedOptional<() -> ()>):
// CHECK-NEXT: [[F:%.*]] = alloc_box $ImplicitlyUnwrappedOptional<() -> ()>
// CHECK-NEXT: store [[T0]] to [[F]]#1
// CHECK:      [[T1:%.*]] = select_enum_addr [[F]]#1
// CHECK-NEXT: cond_br [[T1]], bb1, bb2
//   If it does, project and load the value out of the implicitly unwrapped
//   optional...
// CHECK:    bb1:
// CHECK-NEXT: [[FN0_ADDR:%.*]] = unchecked_take_enum_data_addr [[F]]
// CHECK-NEXT: [[FN0:%.*]] = load [[FN0_ADDR]]
//   ...unnecessarily reabstract back to () -> ()...
// CHECK:      [[T0:%.*]] = function_ref @_TTRXFo_iT__iT__XFo__dT__ : $@convention(thin) (@owned @callee_owned (@out (), @in ()) -> ()) -> ()
// CHECK-NEXT: [[FN1:%.*]] = partial_apply [[T0]]([[FN0]])
//   .... then call it
// CHECK-NEXT: apply [[FN1]]()
// CHECK:      br bb3
//   (first nothing block)
// CHECK:    bb2:
// CHECK-NEXT: enum $Optional<()>, #Optional.None!enumelt
// CHECK-NEXT: br bb3
//   The rest of this is tested in optional.swift

func wrap<T>(x x: T) -> T! { return x }

// CHECK: sil hidden @_TF29implicitly_unwrapped_optional16wrap_then_unwrapU__FT1xQ__Q_
func wrap_then_unwrap<T>(x x: T) -> T {
  // CHECK: [[FORCE:%.*]] = function_ref @_TFSs36_getImplicitlyUnwrappedOptionalValueU__FGSQQ__Q_
  // CHECK: apply [[FORCE]]<{{.*}}>(%0, {{%.*}})
  return wrap(x: x)!
}

// CHECK: sil hidden @_TF29implicitly_unwrapped_optional10tuple_bindFT1xGSQTSiSS___GSqSS_ : $@convention(thin) (@owned ImplicitlyUnwrappedOptional<(Int, String)>) -> @owned Optional<String> {
func tuple_bind(x x: (Int, String)!) -> String? {
  return x?.1
  // CHECK:   cond_br {{%.*}}, [[NONNULL:bb[0-9]+]], [[NULL:bb[0-9]+]]
  // CHECK: [[NONNULL]]:
  // CHECK:   [[STRING:%.*]] = tuple_extract {{%.*}} : $(Int, String), 1
  // CHECK-NOT: release_value [[STRING]]
}

func tuple_bind_implicitly_unwrapped(x x: (Int, String)!) -> String {
  return x.1
}

func return_any() -> AnyObject! { return nil }
func bind_any() {
  let object : AnyObject? = return_any()
}
