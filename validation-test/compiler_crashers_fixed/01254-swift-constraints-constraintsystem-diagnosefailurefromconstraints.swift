// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
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
