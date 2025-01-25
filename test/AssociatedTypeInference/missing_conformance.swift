// RUN: %target-typecheck-verify-swift

// Test candidates for witnesses that are missing conformances
// in various ways.

protocol LikeSetAlgebra {
    func onion(_ other: Self) -> Self // expected-note {{protocol requires function 'onion' with type '(X) -> X'}}
    func indifference(_ other: Self) -> Self // expected-note {{protocol requires function 'indifference' with type '(X) -> X'}}

}
protocol LikeOptionSet : LikeSetAlgebra, RawRepresentable {}
extension LikeOptionSet where RawValue : FixedWidthInteger {
    func onion(_ other: Self) -> Self { return self } // expected-note {{candidate would match if 'X.RawValue' conformed to 'FixedWidthInteger'}}
    func indifference(_ other: Self) -> Self { return self } // expected-note {{candidate would match if 'X.RawValue' conformed to 'FixedWidthInteger'}}
}

struct X : LikeOptionSet {}
// expected-error@-1 {{type 'X' does not conform to protocol 'LikeSetAlgebra'}}
// expected-error@-2 {{type 'X' does not conform to protocol 'RawRepresentable'}}
// expected-note@-3 {{add stubs for conformance}}

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
    func get() -> Result // expected-note {{protocol requires function 'get()' with type '() -> Result'}}
    func got() // expected-note {{protocol requires function 'got()' with type '() -> ()'}}
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

struct Z<T1, T2, T3, Result, T4> : P1 {} // expected-error {{type 'Z<T1, T2, T3, Result, T4>' does not conform to protocol 'P1'}} expected-note {{add stubs for conformance}}

protocol P4 {
    func this() // expected-note 2 {{protocol requires function 'this()' with type '() -> ()'}}
}
protocol P5 {}
extension P4 where Self : P5 {
    func this() {} // expected-note {{candidate would match if 'W' conformed to 'P5'}}
    //// expected-note@-1 {{candidate would match if 'S<T>.SS' conformed to 'P5'}}
}
struct W : P4 {} // expected-error {{type 'W' does not conform to protocol 'P4'}} expected-note {{add stubs for conformance}}

struct S<T> {
    struct SS : P4 {} // expected-error {{type 'S<T>.SS' does not conform to protocol 'P4'}} expected-note {{add stubs for conformance}}
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

protocol P9 {
    func foo() // expected-note {{protocol requires function 'foo()' with type '() -> ()'}}
}
class C2 {}
extension P9 where Self : C2 {
    func foo() {} // expected-note {{candidate would match if 'C3' subclassed 'C2'}}
}
class C3 : P9 {} // expected-error {{type 'C3' does not conform to protocol 'P9'}} expected-note {{add stubs for conformance}}

protocol P10 {
    associatedtype A
    func bar() // expected-note {{protocol requires function 'bar()' with type '() -> ()'}}
}
extension P10 where A == Int {
    func bar() {} // expected-note {{candidate would match if 'A' was the same type as 'Int'}}
}
struct S2<A> : P10 {} // expected-error {{type 'S2<A>' does not conform to protocol 'P10'}} expected-note {{add stubs for conformance}}

protocol P11 {}
protocol P12 {
    associatedtype A : P11 // expected-note {{unable to infer associated type 'A' for protocol 'P12'}}
    func bar() -> A
}
extension Int : P11 {}
struct S3 : P12 { // expected-error {{type 'S3' does not conform to protocol 'P12'}}
    func bar() -> P11 { return 0 }
    // expected-note@-1 {{cannot infer 'A' = 'any P11' because 'any P11' as a type cannot conform to protocols; did you mean to use an opaque result type?}}{{19-19=some }}
}

protocol P13 {
  associatedtype A : P11 // expected-note {{unable to infer associated type 'A' for protocol 'P13'}}
  var bar: A { get }
}
struct S4: P13 { // expected-error {{type 'S4' does not conform to protocol 'P13'}}
  var bar: P11 { return 0 }
  // expected-note@-1 {{cannot infer 'A' = 'any P11' because 'any P11' as a type cannot conform to protocols; did you mean to use an opaque result type?}}{{12-12=some }}
}

protocol P14 {
  associatedtype A : P11 // expected-note {{unable to infer associated type 'A' for protocol 'P14'}}
  subscript(i: Int) -> A { get }
}
struct S5: P14 { // expected-error {{type 'S5' does not conform to protocol 'P14'}}
  subscript(i: Int) -> P11 { return i }
  // expected-note@-1 {{cannot infer 'A' = 'any P11' because 'any P11' as a type cannot conform to protocols; did you mean to use an opaque result type?}}{{24-24=some }}
}

// https://github.com/apple/swift/issues/55204

// Note: the conformance to collection should succeed
struct CountSteps1<T> : Collection {
  init(count: Int) { self.count = count }
  var count: Int

  var startIndex: Int { 0 }
  var endIndex: Int { count }
  func index(after i: Int) -> Int {
    totalSteps += 1 // expected-error {{cannot find 'totalSteps' in scope}}
    return i + 1
  }
  subscript(i: Int) -> Int { return i }
}

extension CountSteps1 // expected-error {{type 'CountSteps1<T>' does not conform to protocol 'RandomAccessCollection'}}
  // expected-note@-1 {{add stubs for conformance}}
  // expected-error@-2 {{conditional conformance of type 'CountSteps1<T>' to protocol 'RandomAccessCollection' does not imply conformance to inherited protocol 'BidirectionalCollection'}}
  // expected-note@-3 {{did you mean to explicitly state the conformance with the same bounds using 'where T : Equatable'?}}
  // expected-note@-4 {{did you mean to explicitly state the conformance with different bounds?}}
  // expected-error@-5 {{type 'CountSteps1<T>' does not conform to protocol 'BidirectionalCollection'}}
  : RandomAccessCollection
     where T : Equatable
{
  typealias Index = Int

  func index(_ i: Index, offsetBy d: Int) -> Index {
    return i + d
  }
}


