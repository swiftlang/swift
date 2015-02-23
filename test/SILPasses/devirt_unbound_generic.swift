// RUN: %target-swift-frontend -emit-sil -O -emit-object %s

// We used to crash on this when trying to devirtualize t.boo(a, 1),
// because it is an "apply" with unbound generic arguments and
// devirtualizer is not able to devirtualize unbound generic
// invocations yet.
//
// rdar://19912272

protocol P {
   typealias Node
}

class C<T:P> {
   typealias Node = T.Node

   func foo(n:Node) {
   }

   func boo<S>(n:Node, s:S) {
   }
}

func test1<T>(t:C<T>, a: T.Node) {
   t.boo(a, s:1)
}
