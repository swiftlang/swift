// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class A {
}
let c: A? = nil
if c == .b {
}
func c<d {
enum c {
func e
var _ = e
