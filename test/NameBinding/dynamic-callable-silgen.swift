// RUN: %target-swift-frontend -emit-silgen -verify %s | %FileCheck %s
 
@dynamicCallable
public struct Callable {
  func dynamicallyCall(withArguments: [Int]) {}
  func dynamicallyCall(withKeywordArguments: DictionaryLiteral<String, Int>) {}
}
 
@_silgen_name("foo")
public func foo(a: Callable) {
  // The first two calls should resolve to the `withArguments:` method.
  a()
  a(1, 2, 3)
  // The last call should resolve to the `withKeywordArguments:` method.
  a(1, 2, 3, label: 4)
}
 
// CHECK-LABEL: sil @foo
// CHECK: bb0(%0 : $Callable):
// CHECK: [[DYN_CALL_1:%.*]] = function_ref @$S4main8CallableV15dynamicallyCall13withArgumentsySaySiG_tF
// CHECK-NEXT: apply [[DYN_CALL_1]]
// CHECK: [[DYN_CALL_2:%.*]] = function_ref @$S4main8CallableV15dynamicallyCall13withArgumentsySaySiG_tF
// CHECK-NEXT: apply [[DYN_CALL_2]]
// CHECK: [[DYN_CALL_3:%.*]] = function_ref @$S4main8CallableV15dynamicallyCall20withKeywordArgumentsys17DictionaryLiteralVySSSiG_tF
// CHECK-NEXT: apply [[DYN_CALL_3]]
