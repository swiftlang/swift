// RUN: %target-typecheck-verify-swift

// This used to crash -- rdar://120388028

struct Node {}

let x = Outer(implementation: Inner<Node>())

protocol Proto {
    associatedtype Node
    func doThings(things: Outer<Node, Self>)
}

struct Outer<Node, Impl: Proto> where Impl.Node == Node {
    var implementation: Impl
}

struct Inner<Node>: Proto { // expected-error {{circular reference}}
// expected-note@-1 {{through reference here}}
    func doThings(things: Outer<Node, Inner<Node>>) {}
    // expected-note@-1 2{{through reference here}}
    // expected-note@-2 {{while resolving type 'Outer<Node, Inner<Node>>'}}
}
