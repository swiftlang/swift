// RUN: %target-typecheck-verify-swift

protocol P {}

struct S<T> {}

class C : P {}

extension S where T : P {
// expected-note@-1 {{where 'T' = 'T'}}
// expected-note@-2 {{where 'T' = 'Int'}}
  typealias A = Int
  typealias B<U> = S<U>
}

extension S where T == Float { // expected-note {{where 'T' = 'Int'}}
  typealias C = Int
}

class A<T, U> {}

extension A where T == [U], U: P { // expected-note {{where 'U' = 'Float'}}
  typealias S1 = Int
}

extension A where T == [U], U == Int {
// expected-note@-1 {{where 'U' = 'String'}}
// expected-note@-2 {{where 'T' = '[String]'}}
  typealias S2 = Int
}

class B<U> : A<[U], U> {}

_ = B<C>.S1()          // Ok
_ = B<Int>.S2()        // Ok
_ = B<Float>.S1()      // expected-error {{referencing type alias 'S1' on 'A' requires that 'Float' conform to 'P'}}
_ = B<String>.S2()
// expected-error@-1 {{referencing type alias 'S2' on 'A' requires the types '[String]' and '[Int]' be equivalent}}
// expected-error@-2 {{referencing type alias 'S2' on 'A' requires the types 'String' and 'Int' be equivalent}}

_ = S<C>.A()           // Ok
_ = S<Int>.A()         // expected-error {{referencing type alias 'A' on 'S' requires that 'Int' conform to 'P'}}
_ = S<String>.B<Int>() // expected-error {{type 'String' does not conform to protocol 'P'}}
_ = S<Int>.C()         // expected-error {{referencing type alias 'C' on 'S' requires the types 'Int' and 'Float' be equivalent}}

func foo<T>(_ s: S<T>.Type) {
  _ = s.A() // expected-error {{referencing type alias 'A' on 'S' requires that 'T' conform to 'P'}}
}

func bar<T: P>(_ s: S<T>.Type) {
  _ = s.A() // Ok
}
