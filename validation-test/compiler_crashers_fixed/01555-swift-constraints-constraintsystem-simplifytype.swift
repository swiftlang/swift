// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol a {
func e> {
}
struct D : (e: B)
}() {
}
protocol B : a {
typealias e = D>() {
