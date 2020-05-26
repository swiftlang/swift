// RUN: %target-swift-frontend -typecheck -verify %s

protocol Nested {
    associatedtype U // expected-note {{protocol requires nested type 'U'; do you want to add it?}}
}

class A<M> {
    func f<T : Nested>(_ t: T, _ keyPath: WritableKeyPath<M, T.U>) {}
}

class B<Y> : Nested { // expected-error {{type 'B<Y>' does not conform to protocol 'Nested'}}
    var i: Y?
}


class C<M> {
    func cFunc(_ a: A<M>) {
        let function: (B<Int>, ReferenceWritableKeyPath<M, Int>) -> Void = a.f // expected-error {{cannot convert value of type '(B<Int>, WritableKeyPath<M, B<Int>.U>) -> ()' to specified type '(B<Int>, ReferenceWritableKeyPath<M, Int>) -> Void'}}
    }
}
