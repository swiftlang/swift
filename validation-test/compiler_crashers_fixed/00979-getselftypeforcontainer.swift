// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol b {
extension String {
}
protocol b : a {
class func a<b
}
}
func a<T -> Any) -> {
class A {
var _ = b
