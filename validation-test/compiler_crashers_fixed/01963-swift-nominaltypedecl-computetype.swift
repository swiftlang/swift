// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol A {
protocol A : a {
self.<Q<T: Sequence, A = c> Int {
}
func a
}
}
let n1: (z: [Int
extension A
