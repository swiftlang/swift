// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func a<T>() {f {
}
class d: f{  class func i {}
func f() {
}
func prefix(with: String) -> <T>(() -> T) -> String {
}
protocol a : a {
}
protocol f {
}
protocol j : f {
}
protocol e : f {
}
protocol i {
}
struct c : i {
}
func i<j : j, d : i j
init(b: c) {
