// RUN: %target-swift-frontend %s -typecheck -verify

func badIs<T>(_ value: Any, anInstanceOf type: T.Type) -> Bool { // expected-note {{'type' declared here}}
    value is type // expected-error {{type-casting operator expects a type on its right-hand side (got: parameter 'type')}}
}

func foo() -> Int {
    return // expected-error {{non-void function should return a value}}
}

func badIs_ifdecl<T>(_ value: Any, anInstanceOf type: T.Type) -> Bool { // expected-note {{'type' declared here}}
    #if true
    value is type // expected-error {{type-casting operator expects a type on its right-hand side (got: parameter 'type')}}
    #endif
}

func foo_ifdecl() -> Int {
    #if true
    return // expected-error {{non-void function should return a value}}
    #endif
}
