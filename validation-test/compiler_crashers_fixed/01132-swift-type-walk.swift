// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct e : d {
func i<j : b, k : d where k.f == j> (n: k) {
}
func i<l : d where l.f = 0) {
}
let c = a
protocol c : b { func b
protocol a {
ss b<h : c, i : c where }
}
func a<b:a
