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

// CHECK: nonisolated extension A.InferenceTest {
// CHECK:   nonisolated(nonsending) public func testInExtension() async
// CHECK:   @concurrent public func testConcurrentInExtension() async
// CHECK: }
nonisolated public extension InferenceTest {
  func testInExtension() async {}
  @concurrent func testConcurrentInExtension() async {}
}

// CHECK: public protocol P {
// CHECK:   nonisolated(nonsending) func testWitness() async
// CHECK: }
public protocol P {
  func testWitness() async
}

// CHECK: public struct WitnessTest : nonisolated A.P {
// CHECK:   nonisolated(nonsending) public func testWitness() async
// CHECK: }
public struct WitnessTest: nonisolated P {
  public func testWitness() async {}
}

// CHECK: nonisolated public class NoinsolatedClassTest {
// CHECK:   nonisolated(nonsending) public func test() async
// CHECK: }
nonisolated public class NoinsolatedClassTest {
  public func test() async {}
}

// CHECK: public typealias F = nonisolated(nonsending) () async -> Swift.Void
public typealias F = () async -> Void
// CHECK: public typealias G<T> = nonisolated(nonsending) () async -> T
public typealias G<T> = () async -> T

// CHECK: public func testTypeAlias(_: @escaping A.F)
public func testTypeAlias(_: @escaping F) {}

// CHECK: public struct TestGenericTypeAlias {
// CHECK:   public subscript<U>(_: nonisolated(nonsending) () async -> U) -> Swift.Bool {
// CHECK:     get
// CHECK:   }
// CHECK: }
public struct TestGenericTypeAlias {
  public subscript<U>(_: G<U>) -> Bool { false }
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
// CHECK: function_ref @$s1A13InferenceTestV15testInExtensionyyYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @in_guaranteed InferenceTest) -> ()
// CHECK: function_ref @$s1A13InferenceTestV25testConcurrentInExtensionyyYaF : $@convention(method) @async (@in_guaranteed InferenceTest) -> ()
// CHECK: } // end sil function '$s6Client13testInference1ty1A0C4TestV_tYaF'
@MainActor
func testInference(t: InferenceTest) async {
    await t.infersAttr()

    t.testNested { _ in }
    t.testNested(dict: [:])

    await t.testInExtension()
    await t.testConcurrentInExtension()
}

// CHECK-LABEL: sil hidden @$s6Client15testAutoclosure1ty1A13InferenceTestV_tYaF : $@convention(thin) @async (@in_guaranteed InferenceTest) -> ()
// CHECK: function_ref @$s1A13InferenceTestV15testAutoclosure6value1ySiyYaYCXK_tYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> Int, @in_guaranteed InferenceTest) -> ()
// CHECK: function_ref @$s1A13InferenceTestV15testAutoclosure6value2ySiyYaYCXK_tYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> Int, @in_guaranteed InferenceTest) -> ()
// CHECK: } // end sil function '$s6Client15testAutoclosure1ty1A13InferenceTestV_tYaF'
func testAutoclosure(t: InferenceTest) async {
    await t.testAutoclosure(value1: 42)
    await t.testAutoclosure(value2: 42)
}

// CHECK-LABEL: sil hidden @$s6Client26testWitnessWithNonisolated1ty1A0C4TestV_tYaF : $@convention(thin) @async (@in_guaranteed WitnessTest) -> ()
// CHECK: function_ref @$s1A11WitnessTestV04testA0yyYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @in_guaranteed WitnessTest) -> ()
// CHECK: } // end sil function '$s6Client26testWitnessWithNonisolated1ty1A0C4TestV_tYaF'
func testWitnessWithNonisolated(t: WitnessTest) async {
  await t.testWitness()
}

// CHECK-LABEL: sil hidden @$s6Client20testNonisolatedClass1ty1A011NoinsolatedD4TestC_tYaF : $@convention(thin) @async (@guaranteed NoinsolatedClassTest) -> ()
// CHECK: class_method {{.*}}, #NoinsolatedClassTest.test : (NoinsolatedClassTest) -> () async -> (), $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed NoinsolatedClassTest) -> ()
// CHECK: } // end sil function '$s6Client20testNonisolatedClass1ty1A011NoinsolatedD4TestC_tYaF'
func testNonisolatedClass(t: NoinsolatedClassTest) async {
  await t.test()
}

// CHECK-LABEL: sil hidden @$s6Client18testTypeAliasParam1ty1A011TestGenericcD0V_tF : $@convention(thin) (@in_guaranteed TestGenericTypeAlias) -> ()
// CHECK: [[CLOSURE:%.*]] = function_ref @$s6Client18testTypeAliasParam1ty1A011TestGenericcD0V_tFyyYaYCcfU_ : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()
// CHECK: [[THICK_CLOSURE:%.*]] = thin_to_thick_function [[CLOSURE]] to $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()
// CHECK: [[TEST_TYPEALIAS:%.*]] = function_ref @$s1A13testTypeAliasyyyyYaYCcF : $@convention(thin) (@guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()) -> ()
// CHECK: apply [[TEST_TYPEALIAS]]([[THICK_CLOSURE]]) : $@convention(thin) (@guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()) -> ()
// CHECK: [[CLOSURE_2:%.*]] = convert_function {{.*}} to $@noescape @async @callee_guaranteed @substituted <τ_0_0> (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> @out τ_0_0 for <Int>
// CHECK: [[SUBSCRIPT:%.*]] = function_ref @$s1A20TestGenericTypeAliasVySbxyYaYCXEcluig : $@convention(method) <τ_0_0> (@guaranteed @noescape @async @callee_guaranteed @substituted <τ_0_0> (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> @out τ_0_0 for <τ_0_0>, @in_guaranteed TestGenericTypeAlias) -> Bool
// CHECK: apply [[SUBSCRIPT]]<Int>([[CLOSURE_2]], {{.*}}) : $@convention(method) <τ_0_0> (@guaranteed @noescape @async @callee_guaranteed @substituted <τ_0_0> (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> @out τ_0_0 for <τ_0_0>, @in_guaranteed TestGenericTypeAlias) -> Bool
// CHECK: } // end sil function '$s6Client18testTypeAliasParam1ty1A011TestGenericcD0V_tF'
func testTypeAliasParam(t: TestGenericTypeAlias) {
  // CHECK: closure #1 in testTypeAliasParam(t:)
  // CHECK: Isolation: caller_isolation_inheriting
  // CHECK-LABEL: sil private @$s6Client18testTypeAliasParam1ty1A011TestGenericcD0V_tFyyYaYCcfU_ : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()
  // CHECK: } // end sil function '$s6Client18testTypeAliasParam1ty1A011TestGenericcD0V_tFyyYaYCcfU_'
  testTypeAlias { }

  // CHECK: // closure #2 in testTypeAliasParam(t:)
  // CHECK: // Isolation: caller_isolation_inheriting
  // CHECK-LABEL: sil private @$s6Client18testTypeAliasParam1ty1A011TestGenericcD0V_tFSiyYaYCXEfU0_ : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> Int
  // CHECK: } // end sil function '$s6Client18testTypeAliasParam1ty1A011TestGenericcD0V_tFSiyYaYCXEfU0_'
  _ = t[{ 42 }]
}
