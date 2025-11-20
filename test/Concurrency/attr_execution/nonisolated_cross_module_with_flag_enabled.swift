// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

/// Build the library A
// RUN: %target-swift-frontend -emit-module %t/src/A.swift \
// RUN:   -module-name A -swift-version 6 -enable-library-evolution \
// RUN:   -enable-upcoming-feature NonisolatedNonsendingByDefault \
// RUN:   -emit-module-path %t/A.swiftmodule \
// RUN:   -emit-module-interface-path %t/A.swiftinterface

// RUN: %FileCheck %t/src/A.swift --input-file %t/A.swiftinterface

// RUN: %target-swift-typecheck-module-from-interface(%t/A.swiftinterface) -module-name A

// Build the client using module
// RUN: %target-swift-emit-sil -verify -module-name Client -I %t %t/src/Client.swift | %FileCheck %t/src/Client.swift

// RUN: rm %t/A.swiftmodule

// Re-build the client using interface
// RUN: %target-swift-emit-sil -verify -module-name Client -I %t %t/src/Client.swift | %FileCheck %t/src/Client.swift

//--- A.swift
@MainActor
public final class Test {
  // CHECK: nonisolated(nonsending) final public func test() async
  public nonisolated func test() async {}

  // CHECK: @_Concurrency.MainActor final public func compute(callback: nonisolated(nonsending) @escaping @Sendable () async -> Swift.Void)
  public func compute(callback: @escaping @Sendable () async -> Void) {}
}

public struct InferenceTest {
  // CHECK: nonisolated(nonsending) public func infersAttr() async
  public func infersAttr() async {}

  // CHECK: public func testNested(callback: @escaping (nonisolated(nonsending) @Sendable () async -> Swift.Void) -> Swift.Void)
  public func testNested(callback: @escaping (@Sendable () async -> Void) -> Void) {}
  // CHECK: public func testNested(dict: [Swift.String : (nonisolated(nonsending) () async -> Swift.Void)?])
  public func testNested(dict: [String: (() async -> Void)?]) {}

  // CHECK: nonisolated(nonsending) public func testAutoclosure(value1 fn: nonisolated(nonsending) @autoclosure () async -> Swift.Int) async
  public func testAutoclosure(value1 fn: @autoclosure () async -> Int) async { await fn() }
  // CHECK: nonisolated(nonsending) public func testAutoclosure(value2 fn: nonisolated(nonsending) @autoclosure () async -> Swift.Int) async
  public func testAutoclosure(value2 fn: nonisolated(nonsending) @autoclosure () async -> Int) async { await fn() }
}

//--- Client.swift
import A

// CHECK-LABEL: sil hidden @$s6Client4test1ty1A4TestC_tYaF : $@convention(thin) @async (@guaranteed Test) -> ()
// CHECK: bb0([[SELF:%.*]] : $Test):
// CHECK:  [[MAIN_ACTOR_EXISTENTIAL:%.*]] = init_existential_ref %4 : $MainActor : $MainActor, $any Actor
// CHECK:  [[ANY_ACTOR:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[MAIN_ACTOR_EXISTENTIAL]]
// CHECK:  [[ANY_ACTOR_CAST:%.*]] = unchecked_bitwise_cast [[ANY_ACTOR]] to $Builtin.ImplicitActor
// CHECK:  [[TEST_METHOD:%.*]] = function_ref @$s1A4TestC4testyyYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed Test) -> ()
// CHECK:  apply [[TEST_METHOD]]([[ANY_ACTOR_CAST]], [[SELF]])
// CHECK: } // end sil function '$s6Client4test1ty1A4TestC_tYaF'
@MainActor
func test(t: Test) async {
  await t.test() // Ok
}

// CHECK-LABEL: sil hidden @$s6Client16testWithCallback1ty1A4TestC_tYaF : $@convention(thin) @async (@guaranteed Test) -> ()
// CHECK: function_ref @$s1A4TestC7compute8callbackyyyYaYbYCc_tF : $@convention(method) (@guaranteed @Sendable @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> (), @guaranteed Test) -> ()
// CHECK: } // end sil function '$s6Client16testWithCallback1ty1A4TestC_tYaF'
@MainActor
func testWithCallback(t: Test) async {
  t.compute(callback: {})
}

// CHECK-LABEL: sil hidden @$s6Client13testInference1ty1A0C4TestV_tYaF : $@convention(thin) @async (@in_guaranteed InferenceTest) -> ()
// CHECK: function_ref @$s1A13InferenceTestV10infersAttryyYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @in_guaranteed InferenceTest) -> ()
// CHECK: function_ref @$s1A13InferenceTestV10testNested8callbackyyyyYaYbYCXEc_tF : $@convention(method) (@guaranteed @callee_guaranteed (@guaranteed @noescape @Sendable @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()) -> (), @in_guaranteed InferenceTest) -> ()
// CHECK: function_ref @$s1A13InferenceTestV10testNested4dictySDySSyyYaYCcSgG_tF : $@convention(method) (@guaranteed Dictionary<String, Optional<nonisolated(nonsending) () async -> ()>>, @in_guaranteed InferenceTest) -> ()
// CHECK: } // end sil function '$s6Client13testInference1ty1A0C4TestV_tYaF'
@MainActor
func testInference(t: InferenceTest) async {
    await t.infersAttr()

    t.testNested { _ in }
    t.testNested(dict: [:])
}

// CHECK-LABEL: sil hidden @$s6Client15testAutoclosure1ty1A13InferenceTestV_tYaF : $@convention(thin) @async (@in_guaranteed InferenceTest) -> ()
// CHECK: function_ref @$s1A13InferenceTestV15testAutoclosure6value1ySiyYaYCXK_tYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> Int, @in_guaranteed InferenceTest) -> ()
// CHECK: function_ref @$s1A13InferenceTestV15testAutoclosure6value2ySiyYaYCXK_tYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> Int, @in_guaranteed InferenceTest) -> ()
// CHECK: } // end sil function '$s6Client15testAutoclosure1ty1A13InferenceTestV_tYaF'
func testAutoclosure(t: InferenceTest) async {
    await t.testAutoclosure(value1: 42)
    await t.testAutoclosure(value2: 42)
}
