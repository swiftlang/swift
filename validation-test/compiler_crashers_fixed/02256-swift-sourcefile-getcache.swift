// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
let b {
class a {
enum A {
}
func b<H : Int>()
}
}
protocol B : a {
func a
