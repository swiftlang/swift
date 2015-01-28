// RUN: not --crash %target-swift-frontend %s -parse
// XFAIL: no_asserts

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

func a(x: Any, y: Any) -> (((Any, Any) -> Any) -> Any) {
return {
}
class A<T : A> {
}
class c {
func b((Any, c))(a: (Any, AnyObj c() -> String {
}
}
protocol c : b { func b
