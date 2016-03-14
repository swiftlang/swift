// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
extension Array {
protocol b = "
}
class a: a {
protocol a : a {
func a<c) {
