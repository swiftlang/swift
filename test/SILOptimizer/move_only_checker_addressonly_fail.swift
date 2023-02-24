// RUN: %target-swift-frontend -enable-experimental-move-only -verify %s -emit-sil

func useValue<T>(_ x: T) {}
func consumeValue<T>(_ x: __owned T) {}

struct GenericAggregate<T> {
    var value: T
}

func test1<T>(_ x: T) {
    @_noImplicitCopy let x2 = x // expected-error {{@_noImplicitCopy can not be used on a generic or existential typed binding or a nominal type containing such typed things}}

    // We are ok with the checker not supporting this today as long as we emit
    // the error msg above.
    consumeValue(x2) // expected-error {{Usage of @noImplicitCopy that the move checker does not know how to check!}}
    consumeValue(x2) // expected-error {{Usage of @noImplicitCopy that the move checker does not know how to check!}}
}
