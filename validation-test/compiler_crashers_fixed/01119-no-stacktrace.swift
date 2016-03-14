// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct A {
func g<U>(h: (A, U) -> U) -> (A, U) -> U {
enum A : String {
case b = ""
}
let c: A? = nil
if c == .b {
