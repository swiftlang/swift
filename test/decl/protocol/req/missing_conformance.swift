// RUN: %target-typecheck-verify-swift

// Test candidates for witnesses that are missing conformances
// in various ways.

protocol LikeSetAlgebra {
    func onion(_ other: Self) -> Self // expected-note {{protocol requires function 'onion' with type '(X) -> X'; do you want to add a stub?}}
    func indifference(_ other: Self) -> Self // expected-note {{protocol requires function 'indifference' with type '(X) -> X'; do you want to add a stub?}}

}
protocol LikeOptionSet : LikeSetAlgebra, RawRepresentable {}
extension LikeOptionSet where RawValue : FixedWidthInteger {
    func onion(_ other: Self) -> Self { return self } // expected-note {{candidate would match if 'X.RawValue' conformed to 'FixedWidthInteger'}}
    func indifference(_ other: Self) -> Self { return self } // expected-note {{candidate would match if 'X.RawValue' conformed to 'FixedWidthInteger'}}
}

struct X : LikeOptionSet {}
// expected-error@-1 {{type 'X' does not conform to protocol 'LikeSetAlgebra'}}
// expected-error@-2 {{type 'X' does not conform to protocol 'RawRepresentable'}}

protocol IterProtocol {}
protocol LikeSequence {
    associatedtype Iter : IterProtocol // expected-note {{unable to infer associated type 'Iter' for protocol 'LikeSequence'}}
    func makeIter() -> Iter
}
extension LikeSequence where Self == Self.Iter {
    func makeIter() -> Self { return self } // expected-note {{candidate would match and infer 'Iter' = 'Y' if 'Y' conformed to 'IterProtocol'}}
}

struct Y : LikeSequence {} // expected-error {{type 'Y' does not conform to protocol 'LikeSequence'}}

protocol P1 {
    associatedtype Result
    func get() -> Result // expected-note {{protocol requires function 'get()' with type '() -> Result'; do you want to add a stub?}}
    func got() // expected-note {{protocol requires function 'got()' with type '() -> ()'; do you want to add a stub?}}
}
protocol P2 {
    static var singularThing: Self { get }
}
extension P1 where Result : P2 {
    func get() -> Result { return Result.singularThing } // expected-note {{candidate would match if 'Result' conformed to 'P2'}}
}
protocol P3 {}
extension P1 where Self : P3 {
    func got() {} // expected-note {{candidate would match if 'Z<T1, T2, T3, Result, T4>' conformed to 'P3'}}
}

struct Z<T1, T2, T3, Result, T4> : P1 {} // expected-error {{type 'Z<T1, T2, T3, Result, T4>' does not conform to protocol 'P1'}}

protocol P4 {
    func this() // expected-note 2 {{protocol requires function 'this()' with type '() -> ()'; do you want to add a stub?}}
}
protocol P5 {}
extension P4 where Self : P5 {
    func this() {} // expected-note {{candidate would match if 'W' conformed to 'P5'}}
    //// expected-note@-1 {{candidate would match if 'S<T>.SS' conformed to 'P5'}}
}
struct W : P4 {} // expected-error {{type 'W' does not conform to protocol 'P4'}}

struct S<T> {
    struct SS : P4 {} // expected-error {{type 'S<T>.SS' does not conform to protocol 'P4'}}
}

class C {}
protocol P6 {
    associatedtype T : C // expected-note {{unable to infer associated type 'T' for protocol 'P6'}}
    func f(t: T)
}

struct A : P6 { // expected-error {{type 'A' does not conform to protocol 'P6'}}
    func f(t: Int) {} // expected-note {{candidate can not infer 'T' = 'Int' because 'Int' is not a class type and so can't inherit from 'C'}}
}

protocol P7 {}
protocol P8 {
    associatedtype T : P7 // expected-note {{unable to infer associated type 'T' for protocol 'P8'}}
    func g(t: T)
}

struct B : P8 { // expected-error {{type 'B' does not conform to protocol 'P8'}}
    func g(t: (Int, String)) {} // expected-note {{candidate can not infer 'T' = '(Int, String)' because '(Int, String)' is not a nominal type and so can't conform to 'P7'}}
}
