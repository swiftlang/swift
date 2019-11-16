// RUN: %target-typecheck-verify-swift

protocol P {}

struct S<T> {}

class C : P {}

extension S where T : P { // expected-note {{where 'T' = 'T'}}
  typealias A = Int
  typealias B<U> = S<U>
}

extension S where T == Float { // expected-note {{requirement specified as 'T' == 'Float' [with T = Int]}}
  typealias C = Int
}

class A<T, U> {}

extension A where T == [U], U: P {
  typealias S1 = Int
}

extension A where T == [U], U == Int {
// expected-note@-1 {{requirement specified as 'T' == '[Int]' [with T = [String]]}}
  typealias S2 = Int
}

class B<U> : A<[U], U> {}

_ = B<C>.S1()          // Ok
_ = B<Int>.S2()        // Ok
_ = B<Float>.S1()      // expected-error {{type 'Float' does not conform to protocol 'P'}}
_ = B<String>.S2()
// expected-error@-1 {{'B<String>.S2' (aka 'Int') requires the types '[String]' and '[Int]' be equivalent}}

_ = S<C>.A()           // Ok
_ = S<Int>.A()         // expected-error {{type 'Int' does not conform to protocol 'P'}}
_ = S<String>.B<Int>() // expected-error {{type 'String' does not conform to protocol 'P'}}
_ = S<Int>.C()         // expected-error {{'S<Int>.C' (aka 'Int') requires the types 'Int' and 'Float' be equivalent}}

func foo<T>(_ s: S<T>.Type) {
  _ = s.A() // expected-error {{referencing type alias 'A' on 'S' requires that 'T' conform to 'P'}}
}

func bar<T: P>(_ s: S<T>.Type) {
  _ = s.A() // Ok
}
