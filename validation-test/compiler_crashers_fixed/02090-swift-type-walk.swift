// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol d {
class A : a {
protocol a {
}
func a<h: A, d: a {
}
}
}
let g : d {
init
