// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

func p(l: Any, g: Any) -> (((Any, Any) -> Any) -> Any) {
return {
(p: (Any, Any) -> Any) -> Any in
func n<n : l,) {
}
struct c<d: SequenceType, b where Optional<b> == d.Generator.Element>
