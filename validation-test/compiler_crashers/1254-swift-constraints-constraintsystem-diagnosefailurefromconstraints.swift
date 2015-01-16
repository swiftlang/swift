// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

protocol A {
typealias B
}
class C<D> {
init <A: A where A.B == D>(e: A.B) {
func p(l: Any, g: Any) -> (((Any, Any) -> Any) -> Any) {
return {
(p: (Any, Any) -> Any) -> Any in
func n<n : l,) {
}
n(e
