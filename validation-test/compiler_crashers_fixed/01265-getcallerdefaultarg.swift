// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct A {
func g<U>(h: (A, U) -> U) -> (A, U) -> U {
return { _, x in return x
func b(c) -> <d>(() -> d) {
}
func a(b: Int = 0) {
}
let c = a
c()
