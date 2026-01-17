// RUN: %target-swift-emit-silgen %s | %FileCheck %s

struct ConcreteError: Error {}

func existentialPassThrough(_ body: () throws(Error) -> Void) throws(Error) {
  try body()
}

func concretePassThrough(_ body: () throws(ConcreteError) -> Void) throws(ConcreteError) {
  try body()
}

func genericPassThrough<X>(_ body: () throws(X) -> Void) throws(X) {
  try body()
}

func genericPassThroughReturn<E>(_ body: @escaping () throws(E) -> Void) -> () throws(E) -> Void {
  body
}

func genericWithDefault<E>(e: E.Type,_ body: () throws(E) -> Void = {}) -> Void {}

func neverThrow() {}
func throwConcrete() throws(ConcreteError) {}
func throwExistential() throws {}

// CHECK-LABEL: sil{{.*}} @$s18typed_throws_thunk5test1yyKF
func test1() throws {
  // No thunk needed here because no indirect error result
  // CHECK-NOT: function_ref thunk for
  // CHECK: [[FN:%[0-9]+]] = function_ref @$s18typed_throws_thunk22existentialPassThroughyyyyKXEKF
  // CHECK-NEXT: try_apply [[FN]]
  try existentialPassThrough(neverThrow)
}

// CHECK-LABEL: sil{{.*}} @$s18typed_throws_thunk5test2yyKF
func test2() throws {
  // No thunk needed here because no indirect error result
  // CHECK-NOT: function_ref thunk for
  // CHECK: [[FN:%[0-9]+]] = function_ref @$s18typed_throws_thunk19concretePassThroughyyyyAA13ConcreteErrorVYKXEADYKF
  // CHECK-NEXT: try_apply [[FN]]
  try concretePassThrough(neverThrow)
}

// CHECK-LABEL: sil{{.*}} @$s18typed_throws_thunk5test3yyF
func test3() {
  // CHECK:      [[T1:%[0-9]+]] = function_ref @$s18typed_throws_thunk10neverThrowyyF
  // CHECK-NEXT: [[T2:%[0-9]+]] = thin_to_thick_function [[T1]]
  // CHECK-NEXT: [[T3:%[0-9]+]] = convert_escape_to_noescape [not_guaranteed] [[T2]]
  // CHECK-NEXT: function_ref thunk for
  // CHECK-NEXT: [[THUNK:%[0-9]+]] = function_ref @$sIg_s5NeverOIegzr_TR
  // CHECK-NEXT: [[T4:%[0-9]+]] = partial_apply [callee_guaranteed] [[THUNK]]([[T3]])
  // CHECK-NEXT: [[T5:%[0-9]+]] = convert_function [[T4]]
  // CHECK-NEXT: [[T6:%[0-9]+]] = convert_escape_to_noescape [not_guaranteed] [[T5]]
  // CHECK-NEXT: // function_ref genericPassThrough<A>(_:)
  // CHECK-NEXT: [[T7:%[0-9]+]] = function_ref @$s18typed_throws_thunk18genericPassThroughyyyyxYKXExYKs5ErrorRzlF
  // CHECK-NEXT: [[T8:%[0-9]+]] = alloc_stack $Never
  // CHECK-NEXT: try_apply [[T7]]<Never>([[T8]], [[T6]])
  genericPassThrough(neverThrow)
}

// CHECK-LABEL: sil{{.*}} @$s18typed_throws_thunk5test4yyyyxYKXEKs5ErrorRzlF
// Indirect -> Indirect should not require a thunk
func test4<T: Error>(_ x: () throws(T) -> Void) throws {
  // CHECK-NOT: function_ref thunk for
  // CHECK: [[FN:%[0-9]+]] = function_ref @$s18typed_throws_thunk18genericPassThroughyyyyxYKXExYKs5ErrorRzlF
  // CHECK: try_apply [[FN]]
  try genericPassThrough(x)
}

// CHECK-LABEL: sil{{.*}} @$s18typed_throws_thunk5test5yyyyxYKXEKs5ErrorRzlF
// Indirect -> Existential should require a thunk
func test5<T: Error>(_ x: () throws(T) -> Void) throws {
  // CHECK: bb0([[T0:%[0-9]+]]
  // CHECK:      [[T3:%[0-9]+]] = copy_value [[T0]]
  // CHECK-NEXT: [[T4:%[0-9]+]] = convert_function [[T3]]
  // CHECK-NEXT: // function_ref thunk for @callee_guaranteed () -> (@error @out A)
  // CHECK-NEXT: [[T5:%[0-9]+]] = function_ref @$sxIgzr_s5Error_pIegzo_sAARzlTR
  // CHECK-NEXT: [[T6:%[0-9]+]] = partial_apply [callee_guaranteed] [[T5]]<T>([[T4]])
  // CHECK-NEXT: [[T7:%[0-9]+]] = convert_escape_to_noescape [not_guaranteed] [[T6]]
  // CHECK-NEXT: // function_ref existentialPassThrough(_:)
  // CHECK-NEXT: [[T8:%[0-9]+]] = function_ref @$s18typed_throws_thunk22existentialPassThroughyyyyKXEKF
  // CHECK-NEXT: try_apply [[T8]]([[T7]])
  try existentialPassThrough(x)
}

// CHECK-LABEL: sil{{.*}} @$s18typed_throws_thunk5test6yyKF
// Concrete -> Existential should require a thunk
func test6() throws {
  // CHECK:      [[T1:%[0-9]+]] = function_ref @$s18typed_throws_thunk13throwConcreteyyAA0E5ErrorVYKF
  // CHECK-NEXT: [[T2:%[0-9]+]] = thin_to_thick_function [[T1]]
  // CHECK-NEXT: // function_ref thunk for @escaping @callee_guaranteed () -> (@error @owned ConcreteError)
  // CHECK-NEXT: [[T3:%[0-9]+]] = function_ref @$s18typed_throws_thunk13ConcreteErrorVIegzo_s0E0_pIegzo_TR
  // CHECK-NEXT: [[T4:%[0-9]+]] = partial_apply [callee_guaranteed] [[T3]]([[T2]])
  // CHECK-NEXT: [[T5:%[0-9]+]] = convert_escape_to_noescape [not_guaranteed] [[T4]]
  // CHECK-NEXT: // function_ref existentialPassThrough(_:)
  // CHECK-NEXT: [[T6:%[0-9]+]] = function_ref @$s18typed_throws_thunk22existentialPassThroughyyyyKXEKF
  // CHECK-NEXT: try_apply [[T6]]([[T5]])
  try existentialPassThrough(throwConcrete)
}

// CHECK-LABEL: sil{{.*}} @$s18typed_throws_thunk5test7yyKF
// Concrete -> Concrete should not require a thunk
func test7() throws {
  // CHECK-NOT: function_ref thunk for
  // CHECK: [[FN:%[0-9]+]] = function_ref @$s18typed_throws_thunk19concretePassThroughyyyyAA13ConcreteErrorVYKXEADYKF
  // CHECK: try_apply [[FN]]
  try concretePassThrough(throwConcrete)
}

// CHECK-LABEL: sil{{.*}} @$s18typed_throws_thunk5test8yyKF
// Concrete -> Indirect should require a thunk
func test8() throws {
  // CHECK:      [[T1:%[0-9]+]] = function_ref @$s18typed_throws_thunk13throwConcreteyyAA0E5ErrorVYKF
  // CHECK-NEXT: [[T2:%[0-9]+]] = thin_to_thick_function [[T1]]
  // CHECK-NEXT: [[T3:%[0-9]+]] = convert_escape_to_noescape [not_guaranteed] [[T2]]
  // CHECK-NEXT: // function_ref thunk for @callee_guaranteed () -> (@error @owned ConcreteError)
  // CHECK-NEXT: [[T4:%[0-9]+]] = function_ref @$s18typed_throws_thunk13ConcreteErrorVIgzo_ACIegzr_TR
  // CHECK-NEXT: [[T5:%[0-9]+]] = partial_apply [callee_guaranteed] [[T4]]([[T3]])
  // CHECK-NEXT: [[T6:%[0-9]+]] = convert_function [[T5]]
  // CHECK-NEXT: [[T7:%[0-9]+]] = convert_escape_to_noescape [not_guaranteed] [[T6]]
  // CHECK-NEXT: // function_ref genericPassThrough<A>(_:)
  // CHECK-NEXT: [[T8:%[0-9]+]] = function_ref @$s18typed_throws_thunk18genericPassThroughyyyyxYKXExYKs5ErrorRzlF
  // CHECK-NEXT: [[T9:%[0-9]+]] = alloc_stack $ConcreteError
  // CHECK-NEXT: try_apply [[T8]]<ConcreteError>([[T9]], [[T7]])
  try genericPassThrough(throwConcrete)
}

// CHECK-LABEL: sil{{.*}} @$s18typed_throws_thunk5test9yyKF
// Existential -> Indirect should require a thunk
func test9() throws {
  // CHECK:      [[T1:%[0-9]+]] = function_ref @$s18typed_throws_thunk16throwExistentialyyKF
  // CHECK-NEXT: [[T2:%[0-9]+]] = thin_to_thick_function [[T1]]
  // CHECK-NEXT: [[T3:%[0-9]+]] = convert_escape_to_noescape [not_guaranteed] [[T2]]
  // CHECK-NEXT: // function_ref thunk for @callee_guaranteed () -> (@error @owned Error)
  // CHECK-NEXT: [[T4:%[0-9]+]] = function_ref @$ss5Error_pIgzo_sAA_pIegzr_TR
  // CHECK-NEXT: [[T5:%[0-9]+]] = partial_apply [callee_guaranteed] [[T4]]([[T3]])
  // CHECK-NEXT: [[T6:%[0-9]+]] = convert_function [[T5]]
  // CHECK-NEXT: [[T7:%[0-9]+]] = convert_escape_to_noescape [not_guaranteed] [[T6]]
  // CHECK-NEXT: // function_ref genericPassThrough<A>(_:)
  // CHECK-NEXT: [[T8:%[0-9]+]] = function_ref @$s18typed_throws_thunk18genericPassThroughyyyyxYKXExYKs5ErrorRzlF
  // CHECK-NEXT: [[T9:%[0-9]+]] = alloc_stack $any Error
  // CHECK-NEXT: try_apply [[T8]]<any Error>([[T9]], [[T7]])
  try genericPassThrough(throwExistential)
}

// https://github.com/swiftlang/swift/issues/77123

// CHECK-LABEL: sil{{.*}} @$s18typed_throws_thunk6test10yyF
// Thunks for throws(Never) <-> nothrow
func test10() {
  // CHECK:   [[NEVER_THROW:%.*]] = function_ref @$s18typed_throws_thunk10neverThrowyyF
  // CHECK:   [[THUNK_TO_NEVER:%.*]] = function_ref @$sIeg_s5NeverOIegzr_TR
  // CHECK:   [[PARTIAL_APPLY_1:%.*]] = partial_apply [callee_guaranteed] [[THUNK_TO_NEVER]]
  // CHECK:   [[GENERIC_PASSTHROUGH:%.*]] = function_ref @$s18typed_throws_thunk24genericPassThroughReturnyyyxYKcyyxYKcs5ErrorRzlF
  // CHECK:   [[RESULT:%.*]] = apply [[GENERIC_PASSTHROUGH]]<Never>
  // CHECK:   [[THUNK_FROM_NEVER:%.*]] = function_ref @$ss5NeverOIegzr_Ieg_TR
  // CHECK:   [[PARTIAL_APPLY_2:%.*]] = partial_apply [callee_guaranteed] [[THUNK_FROM_NEVER]]
  _ = genericPassThroughReturn(neverThrow)
}

// Substituted-to-original Thunk: throws(Never) wrapping nothrow

// CHECK-LABEL: sil {{.*}} [reabstraction_thunk] {{.*}} @$sIeg_s5NeverOIegzr_TR : $@convention(thin) (@guaranteed @callee_guaranteed () -> ()) -> @error_indirect Never
// CHECK: bb0([[ERROR_ADDR:%[0-9]+]] : $*Never, [[CLOSURE:%[0-9]+]] : @guaranteed $@callee_guaranteed () -> ()):
// CHECK:   [[RESULT:%[0-9]+]] = apply [[CLOSURE]]() : $@callee_guaranteed () -> ()
// CHECK:   [[TUPLE:%[0-9]+]] = tuple ()
// CHECK:   return [[TUPLE]]

// Original-to-substituted Thunk: nothrow wrapping throws(Never)

// CHECK-LABEL: sil {{.*}} [reabstraction_thunk] {{.*}} @$ss5NeverOIegzr_Ieg_TR : $@convention(thin) (@guaranteed @callee_guaranteed () -> @error_indirect Never) -> ()
// CHECK: bb0([[CLOSURE:%[0-9]+]] : @guaranteed $@callee_guaranteed () -> @error_indirect Never):
// CHECK:   [[ERROR_ADDR:%[0-9]+]] = alloc_stack $Never
// CHECK:   try_apply [[CLOSURE]]([[ERROR_ADDR]]) : $@callee_guaranteed () -> @error_indirect Never, normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
// CHECK: [[NORMAL_BB]]
// CHECK:   [[TUPLE:%[0-9]+]] = tuple ()
// CHECK:   dealloc_stack [[ERROR_ADDR]]
// CHECK:   return [[TUPLE]]
// CHECK: [[ERROR_BB]]:
// CHECK:   unreachable

// CHECK-LABEL: sil hidden [ossa] @$s18typed_throws_thunk6test11yyF
func test11() {
  // CHECK:   [[DEFAULT_ARG:%.*]] = function_ref @$s18typed_throws_thunk18genericWithDefault1e_yxm_yyxYKXEts5ErrorRzlFfA0_
  // CHECK:   [[DEFAULT_RESULT:%.*]] = apply [[DEFAULT_ARG]]<Never>()
  // CHECK:   [[THUNK_FROM_NEVER:%.*]] = function_ref @$ss5NeverOIegzr_Ieg_TR
  // CHECK:   [[PARTIAL_APPLY_1:%.*]] = partial_apply [callee_guaranteed] [[THUNK_FROM_NEVER]]
  // CHECK:   [[NOESCAPE:%.*]] = convert_escape_to_noescape [not_guaranteed] [[PARTIAL_APPLY_1]]
  // CHECK:   [[THUNK_TO_NEVER:%.*]] = function_ref @$sIg_s5NeverOIegzr_TR
  // CHECK:   [[PARTIAL_APPLY_2:%.*]] = partial_apply [callee_guaranteed] [[THUNK_TO_NEVER]]
  // CHECK:   [[GENERIC_WITH_DEFAULT:%.*]] = function_ref @$s18typed_throws_thunk18genericWithDefault1e_yxm_yyxYKXEts5ErrorRzlF
  // CHECK:   apply [[GENERIC_WITH_DEFAULT]]<Never>
  genericWithDefault(e: Never.self)
}
