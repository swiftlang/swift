// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

/// Build the library A
// RUN: %target-swift-frontend -emit-module %t/src/API.swift \
// RUN:   -module-name API -swift-version 5 -enable-library-evolution \
// RUN:   -emit-module-path %t/API.swiftmodule \
// RUN:   -emit-module-interface-path %t/API.swiftinterface

// Build client with module
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types \
// RUN:   -I %t \
// RUN:   -target %target-swift-5.1-abi-triple \
// RUN:   -module-name Client \
// RUN:   -enable-upcoming-feature DynamicActorIsolation \
// RUN:    %t/src/Client.swift -verify | %FileCheck %s

// Delete swiftmodule to test building against swiftinterface
// RUN: rm %t/API.swiftmodule

// Build client from interface
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types \
// RUN:   -I %t \
// RUN:   -target %target-swift-5.1-abi-triple \
// RUN:   -module-name Client \
// RUN:   -enable-upcoming-feature DynamicActorIsolation \
// RUN:    %t/src/Client.swift -verify | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: swift_feature_DynamicActorIsolation

//--- API.swift
public func compute<T>(_: ((T) -> Void)?) {}

public struct S<T> {
  public init() {}

  public func test<U>(_: ((T) -> U)?) {}

  public subscript(fn: () -> T) -> T {
    get { fn() }
  }
}

//--- Client.swift
import API

@MainActor func test(_: Int?) {
}

// CHECK-LABEL: sil hidden [ossa] @$s6Client20testMainActorContextyyF : $@convention(thin) () -> ()
// CHECK: [[TEST_REF:%.*]] = function_ref @$s6Client4testyySiSgF : $@convention(thin) (Optional<Int>) -> ()
// CHECK-NEXT: [[TEST_FN:%.*]] = thin_to_thick_function [[TEST_REF]] : $@convention(thin) (Optional<Int>) -> () to $@callee_guaranteed (Optional<Int>) -> ()
// CHECK: [[THUNK:%.*]] = function_ref @$sSiSgIegy_AAIegy_TRScMTU : $@convention(thin) (Optional<Int>, @guaranteed @callee_guaranteed (Optional<Int>) -> ()) -> ()
// CHECK-NEXT: [[THUNKED_TEST_REF:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[TEST_FN]]) : $@convention(thin) (Optional<Int>, @guaranteed @callee_guaranteed (Optional<Int>) -> ()) -> ()
// CHECK-NEXT: // function_ref thunk
// CHECK-NEXT: [[REABSTRACTION_THUNK_REF:%.*]] = function_ref @$sSiSgIegy_AAIegn_TR :
// CHECK-NEXT: [[REABSTRACTED_TEST_REF:%.*]] = partial_apply [callee_guaranteed] [[REABSTRACTION_THUNK_REF]]([[THUNKED_TEST_REF]]) : $@convention(thin) (@in_guaranteed Optional<Int>, @guaranteed @callee_guaranteed (Optional<Int>) -> ()) -> ()
// CHECK-NEXT: [[CONVERTED_REABSTRACTED_TEST_REF:%.*]] = convert_function [[REABSTRACTED_TEST_REF]] : $@callee_guaranteed (@in_guaranteed Optional<Int>) -> () to $@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <Optional<Int>>
// CHECK-NEXT: [[OPTIONAL_TEST:%.*]] = enum $Optional<@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <Optional<Int>>>, #Optional.some!enumelt, [[CONVERTED_REABSTRACTED_TEST_REF]] :
// CHECK-NEXT: // function_ref
// CHECK-NEXT: [[COMPUTE_REF:%.*]] = function_ref @$s3API7computeyyyxcSglF : $@convention(thin) <τ_0_0> (@guaranteed Optional<@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <τ_0_0>>) -> ()
// CHECK-NEXT: {{.*}} = apply [[COMPUTE_REF]]<Int?>([[OPTIONAL_TEST]]) : $@convention(thin) <τ_0_0> (@guaranteed Optional<@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <τ_0_0>>) -> ()
@MainActor
func testMainActorContext() {
  compute(test) // no warning
}

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sSiSgIegy_AAIegy_TRScMTU : $@convention(thin) (Optional<Int>, @guaranteed @callee_guaranteed (Optional<Int>) -> ()) -> ()
// CHECK: [[MAIN_ACTOR_METATYPE:%.*]] = metatype $@thick MainActor.Type
// CHECK: [[SHARED_FIELD_GETTER:%.*]] = function_ref @$sScM6sharedScMvgZ : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK-NEXT: [[MAIN_ACTOR:%.*]] = apply [[SHARED_FIELD_GETTER]]([[MAIN_ACTOR_METATYPE]]) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK-NEXT: [[MAIN_ACTOR_ACCESS:%.*]] = begin_borrow [[MAIN_ACTOR]] : $MainActor
// CHECK-NEXT: [[EXEC:%.*]] = extract_executor [[MAIN_ACTOR_ACCESS]] : $MainActor
// CHECK: [[CHECK_EXEC_REF:%.*]] = function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF
// CHECK-NEXT: {{.*}} = apply [[CHECK_EXEC_REF]]({{.*}}, [[EXEC]])


// CHECK-LABEL: sil hidden [ossa] @$s6Client17testComplexGlobalyyyxSgScMYcclF : $@convention(thin) <U> (@guaranteed @callee_guaranteed @substituted <τ_0_0> (@in_guaranteed Optional<τ_0_0>) -> () for <U>) -> ()
func testComplexGlobal<U>(_ fn: @escaping @MainActor (U?) -> Void) {
  // CHECK: [[FN:%.*]] = copy_value %0 : $@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed Optional<τ_0_0>) -> () for <U>
  // CHECK-NEXT: [[SUBST_FN:%.*]] = convert_function [[FN]] : $@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed Optional<τ_0_0>) -> () for <U> to $@callee_guaranteed (@in_guaranteed Optional<U>) -> ()
  // CHECK: [[FN_THUNK:%.*]] = function_ref @$sxSgIegn_AAIegn_lTRScMTU : $@convention(thin) <τ_0_0> (@in_guaranteed Optional<τ_0_0>, @guaranteed @callee_guaranteed (@in_guaranteed Optional<τ_0_0>) -> ()) -> ()
  // CHECK-NEXT: [[THUNKED_FN:%.*]] = partial_apply [callee_guaranteed] [[FN_THUNK]]<U>([[SUBST_FN]]) : $@convention(thin) <τ_0_0> (@in_guaranteed Optional<τ_0_0>, @guaranteed @callee_guaranteed (@in_guaranteed Optional<τ_0_0>) -> ()) -> ()
  // CHECK-NEXT: [[SUBST_THUNKED_FN:%.*]] = convert_function [[THUNKED_FN]] : $@callee_guaranteed (@in_guaranteed Optional<U>) -> () to $@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed Optional<τ_0_0>) -> () for <U>
  // CHECK-NEXT: [[SUBST_THUNKED_FN_2:%.*]] = convert_function [[SUBST_THUNKED_FN]] : $@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed Optional<τ_0_0>) -> () for <U> to $@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <Optional<U>>
  // CHECK-NEXT: {{.*}} = enum $Optional<@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <Optional<U>>>, #Optional.some!enumelt, [[SUBST_THUNKED_FN_2]] : $@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <Optional<U>>
  compute(fn)
  // expected-warning@-1 {{converting function value of type '@MainActor (U?) -> Void' to '(U?) -> Void' loses global actor 'MainActor'; this is an error in the Swift 6 language mode}}

  // CHECK: [[CLOSURE:%.*]] = function_ref @$s6Client17testComplexGlobalyyyxSgScMYcclFySiScMYccfU_ : $@convention(thin) (Int) -> ()
  // CHECK-NEXT: [[CLOSURE_REF:%.*]] = thin_to_thick_function [[CLOSURE]] : $@convention(thin) (Int) -> () to $@callee_guaranteed (Int) -> ()
  // CHECK: [[CLOSURE_THUNK:%.*]] = function_ref @$sSiIegy_SiIegy_TRScMTU : $@convention(thin) (Int, @guaranteed @callee_guaranteed (Int) -> ()) -> ()
  // CHECK-NEXT: {{.*}} = partial_apply [callee_guaranteed] [[CLOSURE_THUNK]]([[CLOSURE_REF]]) : $@convention(thin) (Int, @guaranteed @callee_guaranteed (Int) -> ()) -> ()
  compute { @MainActor (arg: Int) -> Void in
    // expected-warning@-1 {{converting function value of type '@MainActor (Int) -> Void' to '(Int) -> Void' loses global actor 'MainActor'; this is an error in the Swift 6 language mode}}
  }
}

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sxSgIegn_AAIegn_lTRScMTU : $@convention(thin) <U> (@in_guaranteed Optional<U>, @guaranteed @callee_guaranteed (@in_guaranteed Optional<U>) -> ()) -> ()
// CHECK: [[MAIN_ACTOR_METATYPE:%.*]] = metatype $@thick MainActor.Type
// CHECK: [[SHARED_FIELD_GETTER:%.*]] = function_ref @$sScM6sharedScMvgZ : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK-NEXT: [[MAIN_ACTOR:%.*]] = apply [[SHARED_FIELD_GETTER]]([[MAIN_ACTOR_METATYPE]]) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK-NEXT: [[MAIN_ACTOR_ACCESS:%.*]] = begin_borrow [[MAIN_ACTOR]] : $MainActor
// CHECK-NEXT: [[EXEC:%.*]] = extract_executor [[MAIN_ACTOR_ACCESS]] : $MainActor
// CHECK: [[CHECK_EXEC_REF:%.*]] = function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF
// CHECK-NEXT: {{.*}} = apply [[CHECK_EXEC_REF]]({{.*}}, [[EXEC]])

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sSiIegy_SiIegy_TRScMTU : $@convention(thin) (Int, @guaranteed @callee_guaranteed (Int) -> ()) -> ()
// CHECK: [[MAIN_ACTOR_METATYPE:%.*]] = metatype $@thick MainActor.Type
// CHECK: [[SHARED_FIELD_GETTER:%.*]] = function_ref @$sScM6sharedScMvgZ : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK-NEXT: [[MAIN_ACTOR:%.*]] = apply [[SHARED_FIELD_GETTER]]([[MAIN_ACTOR_METATYPE]]) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK-NEXT: [[MAIN_ACTOR_ACCESS:%.*]] = begin_borrow [[MAIN_ACTOR]] : $MainActor
// CHECK-NEXT: [[EXEC:%.*]] = extract_executor [[MAIN_ACTOR_ACCESS]] : $MainActor
// CHECK: [[CHECK_EXEC_REF:%.*]] = function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF
// CHECK-NEXT: {{.*}} = apply [[CHECK_EXEC_REF]]({{.*}}, [[EXEC]])

// CHECK-LABEL: sil hidden [ossa] @$s6Client11testMembers1v1s2fnyx_3API1SVyxGSixScMYcctlF : $@convention(thin) <X> (@in_guaranteed X, @in_guaranteed S<X>, @guaranteed @callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> Int for <X>) -> ()
@MainActor
func testMembers<X>(v: X, s: S<X>, fn: @escaping @MainActor (X) -> Int) {
  // CHECK: [[FN_COPY:%.*]] = copy_value %2 : $@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> Int for <X>
  // CHECK-NEXT: [[FN:%.*]] = convert_function [[FN_COPY]] : $@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> Int for <X> to $@callee_guaranteed (@in_guaranteed X) -> Int
  // CHECK: [[FN_THUNK:%.*]] = function_ref @$sxSiIegnd_xSiIegnd_lTRScMTU : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0, @guaranteed @callee_guaranteed (@in_guaranteed τ_0_0) -> Int) -> Int
  // CHECK-NEXT: [[THUNKED_FN:%.*]] = partial_apply [callee_guaranteed] [[FN_THUNK]]<X>([[FN]]) : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0, @guaranteed @callee_guaranteed (@in_guaranteed τ_0_0) -> Int) -> Int
  // CHECK-NEXT: {{.*}} = convert_function [[THUNKED_FN]] : $@callee_guaranteed (@in_guaranteed X) -> Int to $@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> Int for <X>
  s.test(fn)

  // CHECK: [[CLOSURE_REF:%.*]] = function_ref @$s6Client11testMembers1v1s2fnyx_3API1SVyxGSixScMYcctlFxyScMYcXEfU_ : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> @out τ_0_0
  // CHECK-NEXT: [[V:%.*]] = alloc_stack $X
  // CHECK-NEXT: copy_addr %0 to [init] [[V]] : $*X
  // CHECK-NEXT: [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[CLOSURE_REF]]<X>([[V]]) : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> @out τ_0_0
  // CHECK-NEXT: [[SUBST_CLOSURE:%.*]] = convert_function [[CLOSURE]] : $@callee_guaranteed () -> @out X to $@callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <X>
  // CHECK-NEXT: [[NOESCAPE_SUBST_CLOSURE:%.*]] = convert_escape_to_noescape [not_guaranteed] [[SUBST_CLOSURE]] : $@callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <X> to $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <X>
  // CHECK-NEXT: [[CLOSURE:%.*]] = convert_function [[NOESCAPE_SUBST_CLOSURE]] : $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <X> to $@noescape @callee_guaranteed () -> @out X
  // CHECK: [[CLOSURE_THUNK_REF:%.*]] = function_ref @$sxIgr_xIgr_lTRScMTU : $@convention(thin) <τ_0_0> (@guaranteed @noescape @callee_guaranteed () -> @out τ_0_0) -> @out τ_0_0
  // CHECK-NEXT: [[THUNKED_CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[CLOSURE_THUNK_REF]]<X>([[CLOSURE]]) : $@convention(thin) <τ_0_0> (@guaranteed @noescape @callee_guaranteed () -> @out τ_0_0) -> @out τ_0_0
  // CHECK-NEXT: [[SUBST_THUNKED_CLOSURE:%.*]] = convert_function [[THUNKED_CLOSURE]] : $@callee_guaranteed () -> @out X to $@callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <X>
  // CHECK-NEXT: [[THUNKED_CLOSURE:%.*]] = convert_escape_to_noescape [not_guaranteed] [[SUBST_THUNKED_CLOSURE]] : $@callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <X> to $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <X>
  // CHECK: [[SETTER_REF:%.*]] = function_ref @$s3API1SVyxxyXEcig : $@convention(method) <τ_0_0> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <τ_0_0>, @in_guaranteed S<τ_0_0>) -> @out τ_0_0
  // CHECK-NEXT: {{.*}} = apply [[SETTER_REF]]<X>({{.*}}, [[THUNKED_CLOSURE]], {{.*}}) : $@convention(method) <τ_0_0> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <τ_0_0>, @in_guaranteed S<τ_0_0>) -> @out τ_0_0
  _ = s[{ @MainActor in v }]

  // CHECK: [[TEST_REF:%.*]] = function_ref @$s6Client4testyySiSgF : $@convention(thin) (Optional<Int>) -> ()
  // CHECK-NEXT: [[TEST:%.*]] = thin_to_thick_function [[TEST_REF]] : $@convention(thin) (Optional<Int>) -> () to $@callee_guaranteed (Optional<Int>) -> ()
  // CHECK: [[TEST_THUNK_REF:%.*]] = function_ref @$sSiSgIegy_AAIegy_TRScMTU : $@convention(thin) (Optional<Int>, @guaranteed @callee_guaranteed (Optional<Int>) -> ()) -> ()
  // CHECK-NEXT: {{.*}} = partial_apply [callee_guaranteed] [[TEST_THUNK_REF]]([[TEST]]) : $@convention(thin) (Optional<Int>, @guaranteed @callee_guaranteed (Optional<Int>) -> ()) -> ()
  S().test(test)
}

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sxSiIegnd_xSiIegnd_lTRScMTU : $@convention(thin) <X> (@in_guaranteed X, @guaranteed @callee_guaranteed (@in_guaranteed X) -> Int) -> Int
// CHECK: [[MAIN_ACTOR_METATYPE:%.*]] = metatype $@thick MainActor.Type
// CHECK: [[SHARED_FIELD_GETTER:%.*]] = function_ref @$sScM6sharedScMvgZ : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK-NEXT: [[MAIN_ACTOR:%.*]] = apply [[SHARED_FIELD_GETTER]]([[MAIN_ACTOR_METATYPE]]) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK-NEXT: [[MAIN_ACTOR_ACCESS:%.*]] = begin_borrow [[MAIN_ACTOR]] : $MainActor
// CHECK-NEXT: [[EXEC:%.*]] = extract_executor [[MAIN_ACTOR_ACCESS]] : $MainActor
// CHECK: [[CHECK_EXEC_REF:%.*]] = function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF
// CHECK-NEXT: {{.*}} = apply [[CHECK_EXEC_REF]]({{.*}}, [[EXEC]])

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sxIgr_xIgr_lTRScMTU : $@convention(thin) <X> (@guaranteed @noescape @callee_guaranteed () -> @out X) -> @out X
// CHECK: [[MAIN_ACTOR_METATYPE:%.*]] = metatype $@thick MainActor.Type
// CHECK: [[SHARED_FIELD_GETTER:%.*]] = function_ref @$sScM6sharedScMvgZ : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK-NEXT: [[MAIN_ACTOR:%.*]] = apply [[SHARED_FIELD_GETTER]]([[MAIN_ACTOR_METATYPE]]) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK-NEXT: [[MAIN_ACTOR_ACCESS:%.*]] = begin_borrow [[MAIN_ACTOR]] : $MainActor
// CHECK-NEXT: [[EXEC:%.*]] = extract_executor [[MAIN_ACTOR_ACCESS]] : $MainActor
// CHECK: [[CHECK_EXEC_REF:%.*]] = function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF
// CHECK-NEXT: {{.*}} = apply [[CHECK_EXEC_REF]]({{.*}}, [[EXEC]])
