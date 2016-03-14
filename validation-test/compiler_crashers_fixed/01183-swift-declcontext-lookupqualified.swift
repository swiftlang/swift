// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class c {
func b((Any, c))(Any, AnyObject
}
struct A<T> {
}
struct c<S: Sequence, T where Optional<T> == S.Iterator.Element>(xs
