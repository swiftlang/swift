// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
extension String {
struct S : S<B : b
}
class A<T where T : T> : c
