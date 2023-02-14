// RUN: %target-swift-frontend -enable-experimental-move-only -verify %s -emit-sil

func useValue<T>(_ x: T) {}
func consumeValue<T>(_ x: __owned T) {}

struct GenericAggregate<T> {
    var value: T
}

func test1<T>(_ x: T) {
    @_noImplicitCopy let x2 = x // expected-error {{@_noImplicitCopy can not be used on a generic or existential typed binding or a nominal type containing such typed things}}
    consumeValue(x2)
    consumeValue(x2)
}
REQUIRES: updating_for_owned_noescape
