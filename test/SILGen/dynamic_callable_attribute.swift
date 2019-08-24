// RUN: %target-swift-emit-silgen -verify %s | %FileCheck %s

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
