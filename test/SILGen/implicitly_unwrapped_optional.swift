// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

func foo(f f: (() -> ())!) {
  var f: (() -> ())! = f
  f?()
}
// CHECK: sil hidden @{{.*}}foo{{.*}} : $@convention(thin) (@owned Optional<@callee_owned () -> ()>) -> () {
// CHECK: bb0([[T0:%.*]] : $Optional<@callee_owned () -> ()>):
// CHECK:   [[F:%.*]] = alloc_box ${ var Optional<@callee_owned () -> ()> }
// CHECK:   [[PF:%.*]] = project_box [[F]]
// CHECK:   [[BORROWED_T0:%.*]] = begin_borrow [[T0]]
// CHECK:   [[T0_COPY:%.*]] = copy_value [[BORROWED_T0]]
// CHECK:   store [[T0_COPY]] to [init] [[PF]]
// CHECK:   end_borrow [[BORROWED_T0]] from [[T0]]
// CHECK:   [[T1:%.*]] = select_enum_addr [[PF]]
// CHECK:   cond_br [[T1]], bb1, bb3
//   If it does, project and load the value out of the implicitly unwrapped
//   optional...
// CHECK:    bb1:
// CHECK-NEXT: [[FN0_ADDR:%.*]] = unchecked_take_enum_data_addr [[PF]]
// CHECK-NEXT: [[FN0:%.*]] = load [copy] [[FN0_ADDR]]
//   .... then call it
// CHECK:   apply [[FN0]]() : $@callee_owned () -> ()
// CHECK:   br bb2
// CHECK: bb2(
// CHECK:   destroy_value [[F]]
// CHECK:   destroy_value [[T0]]
// CHECK:   return
// CHECK: bb3:
// CHECK:   enum $Optional<()>, #Optional.none!enumelt
// CHECK:   br bb2
//   The rest of this is tested in optional.swift
// } // end sil function '{{.*}}foo{{.*}}'

func wrap<T>(x x: T) -> T! { return x }

// CHECK-LABEL: sil hidden @_T029implicitly_unwrapped_optional16wrap_then_unwrap{{[_0-9a-zA-Z]*}}F
func wrap_then_unwrap<T>(x x: T) -> T {
  // CHECK:   switch_enum_addr {{%.*}}, case #Optional.none!enumelt: [[FAIL:.*]], default [[OK:bb[0-9]+]]
  // CHECK: [[FAIL]]:
  // CHECK:   unreachable
  // CHECK: [[OK]]:
  return wrap(x: x)!
}

// CHECK-LABEL: sil hidden @_T029implicitly_unwrapped_optional10tuple_bindSSSgSQySi_SStG1x_tF : $@convention(thin) (@owned Optional<(Int, String)>) -> @owned Optional<String> {
func tuple_bind(x x: (Int, String)!) -> String? {
  return x?.1
  // CHECK:   cond_br {{%.*}}, [[NONNULL:bb[0-9]+]], [[NULL:bb[0-9]+]]
  // CHECK: [[NONNULL]]:
  // CHECK:   [[STRING:%.*]] = tuple_extract {{%.*}} : $(Int, String), 1
  // CHECK-NOT: destroy_value [[STRING]]
}

// CHECK-LABEL: sil hidden @_T029implicitly_unwrapped_optional011tuple_bind_a1_B0SSSQySi_SStG1x_tF
func tuple_bind_implicitly_unwrapped(x x: (Int, String)!) -> String {
  return x.1
}

func return_any() -> AnyObject! { return nil }
func bind_any() {
  let object : AnyObject? = return_any()
}

// CHECK-LABEL: sil hidden @_T029implicitly_unwrapped_optional6sr3758yyF
func sr3758() {
  // Verify that there are no additional reabstractions introduced.
  // CHECK: [[CLOSURE:%.+]] = function_ref @_T029implicitly_unwrapped_optional6sr3758yyFySQyypGcfU_ : $@convention(thin) (@in Optional<Any>) -> ()
  // CHECK: [[F:%.+]] = thin_to_thick_function [[CLOSURE]] : $@convention(thin) (@in Optional<Any>) -> () to $@callee_owned (@in Optional<Any>) -> ()
  // CHECK: [[BORROWED_F:%.*]] = begin_borrow [[F]]
  // CHECK: [[CALLEE:%.+]] = copy_value [[BORROWED_F]] : $@callee_owned (@in Optional<Any>) -> ()
  // CHECK: = apply [[CALLEE]]({{%.+}}) : $@callee_owned (@in Optional<Any>) -> ()
  // CHECK: end_borrow [[BORROWED_F]] from [[F]]
  // CHECK: destroy_value [[F]]
  let f: ((Any?) -> Void) = { (arg: Any!) in }
  f(nil)
} // CHECK: end sil function '_T029implicitly_unwrapped_optional6sr3758yyF'
