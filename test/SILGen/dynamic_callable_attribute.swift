// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -verify %s | %FileCheck %s

// Check that dynamic calls resolve to the right `dynamicallyCall` method in SIL.

@dynamicCallable
public struct Callable {
  func dynamicallyCall(withArguments: [Int]) {}
  func dynamicallyCall(withKeywordArguments: KeyValuePairs<String, Int>) {}
}

@_silgen_name("foo")
public func foo(a: Callable) {
  // The first two calls should resolve to the `withArguments:` method.
  a()
  a(1, 2, 3)
  // The last call should resolve to the `withKeywordArguments:` method.
  a(1, 2, 3, label: 4)
}

// CHECK-LABEL: sil [ossa] @foo
// CHECK: bb0(%0 : $Callable):
// CHECK: [[DYN_CALL_1:%.*]] = function_ref @$s26dynamic_callable_attribute8CallableV15dynamicallyCall13withArgumentsySaySiG_tF
// CHECK-NEXT: apply [[DYN_CALL_1]]
// CHECK: [[DYN_CALL_2:%.*]] = function_ref @$s26dynamic_callable_attribute8CallableV15dynamicallyCall13withArgumentsySaySiG_tF
// CHECK-NEXT: apply [[DYN_CALL_2]]
// CHECK: [[DYN_CALL_3:%.*]] = function_ref @$s26dynamic_callable_attribute8CallableV15dynamicallyCall20withKeywordArgumentsys13KeyValuePairsVySSSiG_tF


@dynamicCallable
public struct Callable2 {
  func dynamicallyCall(withKeywordArguments: KeyValuePairs<String, Any>) {}
}

// CHECK-LABEL: sil [ossa] @keywordCoerceBug
// CHECK:[[DYN_CALL:%.*]] = function_ref @$s26dynamic_callable_attribute9Callable2V15dynamicallyCall20withKeywordArgumentsys13KeyValuePairsVySSypG_tF

@_silgen_name("keywordCoerceBug")
public func keywordCoerceBug(a: Callable2, s: Int) {
  a(s)
}

@dynamicCallable
struct S {
  func dynamicallyCall(withArguments x: [Int]) -> Int! { nil }
}

@dynamicCallable
protocol P1 {
  func dynamicallyCall(withKeywordArguments: [String: Any])
}

@dynamicCallable
protocol P2 {
  func dynamicallyCall(withArguments x: [Int]) -> Self
}

@dynamicCallable
class C {
  func dynamicallyCall(withKeywordArguments x: [String: String]) -> Self { return self }
}

// CHECK-LABEL: sil hidden [ossa] @$s26dynamic_callable_attribute05test_A10_callablesyyAA1SV_AA2P1_pAA2P2_pxtAA1CCRbzlF : $@convention(thin) <T where T : C> (S, @in_guaranteed any P1, @in_guaranteed any P2, @guaranteed T) -> ()
func test_dynamic_callables<T : C>(_ s: S, _ p1: P1, _ p2: P2, _ t: T) {
  // https://github.com/apple/swift/issues/55059
  // Compiler crash on '@dynamicCallable' IUO.
  //
  // CHECK: function_ref @$s26dynamic_callable_attribute1SV15dynamicallyCall13withArgumentsSiSgSaySiG_tF : $@convention(method) (@guaranteed Array<Int>, S) -> Optional<Int>
  // CHECK: switch_enum %{{.+}} : $Optional<Int>
  let _: Int = s(0)

  // CHECK: witness_method $@opened({{.+}} any P1) Self, #P1.dynamicallyCall : <Self where Self : P1> (Self) -> ([String : Any]) -> ()
  p1(x: 5)

  // CHECK: witness_method $@opened({{.+}} any P2) Self, #P2.dynamicallyCall : <Self where Self : P2> (Self) -> ([Int]) -> Self
  _ = p2()

  // CHECK: class_method %{{.+}} : $C, #C.dynamicallyCall : (C) -> ([String : String]) -> @dynamic_self C, $@convention(method) (@guaranteed Dictionary<String, String>, @guaranteed C) -> @owned C
  // CHECK: unchecked_ref_cast %{{.+}} : $C to $T
  _ = t("")
}
