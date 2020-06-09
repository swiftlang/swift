// RUN: %target-swift-frontend %s -typecheck -verify

func badIs<T>(_ value: Any, anInstanceOf type: T.Type) -> Bool {
    value is type // expected-error {{cannot find type 'type' in scope}}
}

func foo() -> Int {
    return // expected-error {{non-void function should return a value}}
}
