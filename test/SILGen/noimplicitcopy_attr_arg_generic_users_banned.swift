// RUN: %target-swift-emit-sil -enable-experimental-move-only -parse-stdlib -disable-availability-checking -verify %s

import Swift

class Klass {}


struct MyStruct {
    // Error if @_noImplicitCopy on struct fields. We do not have move only types and
    // these are part of MyStruct.
    //
    // TODO: Make error specific for move only on struct/enum.
    func foo<T>(@_noImplicitCopy _ t: T) { // expected-error {{'@_noImplicitCopy' can not be used on a generic or existential typed binding or a nominal type containing such typed things}}
    }
}

struct MyGenericStruct<T> {
    func foo(@_noImplicitCopy _ t: T) { // expected-error {{'@_noImplicitCopy' can not be used on a generic or existential typed binding or a nominal type containing such typed things}}
    }
}

func foo<T>(@_noImplicitCopy _ t: T) { // expected-error {{'@_noImplicitCopy' can not be used on a generic or existential typed binding or a nominal type containing such typed things}}
}

class MyClass {
    func foo<T>(@_noImplicitCopy _ t: T) { // expected-error {{'@_noImplicitCopy' can not be used on a generic or existential typed binding or a nominal type containing such typed things}}
    }
}

class MyGenericClass<T> {
    func foo(@_noImplicitCopy _ t: T) { // expected-error {{'@_noImplicitCopy' can not be used on a generic or existential typed binding or a nominal type containing such typed things}}
    }
}
