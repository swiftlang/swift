// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class b<h : c, i : c where h.g == i> : a {
}
class b<h, i> {
}
protocol c {
}
class a {
}
struct c<S: Sequence, T where Optional<T> == S.Iterator.Element>(
