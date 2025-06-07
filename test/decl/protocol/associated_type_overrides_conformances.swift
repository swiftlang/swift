// RUN: %target-typecheck-verify-swift

func assertTypeWitnessForP1_A<T: P1, U>(in: T.Type, is: U.Type) where T.A == U {}
func assertTypeWitnessForP2_A<T: P2, U>(in: T.Type, is: U.Type) where T.A == U {}

protocol P1 {
  associatedtype A
  // expected-note@-1 2 {{protocol requires nested type 'A'}}
  // expected-note@-2 2 {{multiple matching types named 'A'}}
}
protocol P2: P1 {
  associatedtype A
  // expected-note@-1 2 {{protocol requires nested type 'A'}}
  // expected-note@-2 2 {{multiple matching types named 'A'}}
}

// Conformance to P1 checked first and more restrictive.
struct S1<T> {}
extension S1: P1 where T == Never { // expected-error {{type 'S1<T>' does not conform to protocol 'P1'}}
  typealias A = Int // expected-note {{possibly intended match}}
}
extension S1: P2 {
  // expected-error@-1 {{type 'S1<T>' does not conform to protocol 'P2'}}
  // expected-error@-2 {{'P2' requires the types 'T' and 'Never' be equivalent}}
  // expected-note@-3 {{requirement specified as 'T' == 'Never'}}
  // expected-note@-4 {{requirement from conditional conformance of 'S1<T>' to 'P1'}}
  typealias A = Bool // expected-note {{possibly intended match}}
}

struct S2<T> {}
extension S2: P1 where T == Never {
  typealias A = Int
}
extension S2: P2 {} // expected-error {{type 'S2<T>' does not conform to protocol 'P2'}} expected-note {{add stubs for conformance}}

struct S3<T> {}
extension S3: P1 where T == Never {}
extension S3: P2 {
  // expected-error@-1 {{type 'S3<T>' does not conform to protocol 'P2'}}
  // expected-error@-2 {{'P2' requires the types 'T' and 'Never' be equivalent}}
  // expected-note@-3 {{requirement specified as 'T' == 'Never'}}
  // expected-note@-4 {{requirement from conditional conformance of 'S3<T>' to 'P1'}}
  typealias A = Int
}

// Conformance to P1 checked first and less restrictive.
struct S4<T> {}
extension S4: P1 {
  typealias A = Int // expected-note {{possibly intended match}}
}
extension S4: P2 where T == Never { // expected-error {{type 'S4<T>' does not conform to protocol 'P2'}}
  typealias A = Bool // expected-note {{possibly intended match}}
}

struct S5<T> {}
extension S5: P1 {
  typealias A = Int
}
extension S5: P2 where T == Never {}

struct S6<T> {}
extension S6: P1 {} // expected-error {{type 'S6<T>' does not conform to protocol 'P1'}} expected-note {{add stubs for conformance}}
extension S6: P2 where T == Never {
  typealias A = Bool
}

// Conformance to P2 checked first and more restrictive.
struct S7<T> {}
extension S7: P2 where T == Never { // expected-error {{type 'S7<T>' does not conform to protocol 'P2'}}
  typealias A = Bool // expected-note {{possibly intended match}}
}
extension S7: P1 {
  typealias A = Int // expected-note {{possibly intended match}}
}

struct S8<T> {}
extension S8: P2 where T == Never {}
extension S8: P1 {
  typealias A = Int
}

struct S9<T> {}
extension S9: P2 where T == Never {
  typealias A = Bool
}
extension S9: P1 {} // expected-error {{type 'S9<T>' does not conform to protocol 'P1'}} expected-note {{add stubs for conformance}}

// Conformance to P2 checked first and less restrictive.
struct S10<T> {}
extension S10: P2 {
  // expected-error@-1 {{type 'S10<T>' does not conform to protocol 'P2'}}
  // expected-error@-2 {{'P2' requires the types 'T' and 'Never' be equivalent}}
  // expected-note@-3 {{requirement specified as 'T' == 'Never'}}
  // expected-note@-4 {{requirement from conditional conformance of 'S10<T>' to 'P1'}}
  typealias A = Bool // expected-note {{possibly intended match}}
}
extension S10: P1 where T == Never { // expected-error {{type 'S10<T>' does not conform to protocol 'P1'}}
  typealias A = Int // expected-note {{possibly intended match}}
}

struct S11<T> {}
extension S11: P2 {} // expected-error {{type 'S11<T>' does not conform to protocol 'P2'}} expected-note {{add stubs for conformance}}
extension S11: P1 where T == Never {
  typealias A = Int
}

struct S12<T> {}
extension S12: P2 {
  // expected-error@-1 {{type 'S12<T>' does not conform to protocol 'P2'}}
  // expected-error@-2 {{'P2' requires the types 'T' and 'Never' be equivalent}}
  // expected-note@-3 {{requirement specified as 'T' == 'Never'}}
  // expected-note@-4 {{requirement from conditional conformance of 'S12<T>' to 'P1'}}
  typealias A = Int
}
extension S12: P1 where T == Never {}

// Inherited conformances.
class Base1 {}
class Derived1: Base1 {}
extension Base1: P1 {
  typealias A = Bool
}
extension Derived1: P2 {
  typealias A = Int
}
assertTypeWitnessForP1_A(in: Base1.self, is: Bool.self)
assertTypeWitnessForP2_A(in: Derived1.self, is: Bool.self)

class Base2 {}
class Derived2: Base2 {}
extension Derived2: P2 {
  typealias A = Int
}
extension Base2: P1 {
  typealias A = Bool
}
assertTypeWitnessForP1_A(in: Base2.self, is: Bool.self)
assertTypeWitnessForP2_A(in: Derived2.self, is: Bool.self)
