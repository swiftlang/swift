// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func a<i>() {
b b {
}
protocol b {
typealias k
}
struct j<n : b> : b {
c == .b {
}
enum S<T> : P {
func f<T>() -> T -> T {
}
protocol P {
func f<T>()(T) -> T
