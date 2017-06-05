// RUN: %target-swift-frontend -typecheck -verify %s

// Ensure that key path exprs can tolerate being re-type-checked when necessary
// to diagnose other errors in adjacent exprs.

struct P<T: K> { }

struct S {
    init<B>(_ a: P<B>) {
        fatalError()
    }
}

protocol K { }

func + <Object>(lhs: KeyPath<A, Object>, rhs: String) -> P<Object> {
    fatalError()
}

// expected-error@+1{{}}
func + (lhs: KeyPath<A, String>, rhs: String) -> P<String> {
    fatalError()
}

struct A {
    let id: String
}

extension A: K {
    static let j = S(\A.id + "id")
}
