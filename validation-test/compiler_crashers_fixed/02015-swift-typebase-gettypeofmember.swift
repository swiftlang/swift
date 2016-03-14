// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol A {
typealias B : I.join(self.init(A, U, "))->()
}
extension A {
enum B == c> {
}
}
class B : A,
