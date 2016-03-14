// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class B<I : B() -> T -> V, f: String {
struct e = {
}
deinit {
enum S<T where I.B : T
