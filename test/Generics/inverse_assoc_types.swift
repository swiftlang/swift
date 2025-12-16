// RUN: %target-typecheck-verify-swift  -enable-experimental-feature SuppressedAssociatedTypesWithDefaults

// REQUIRES: swift_feature_SuppressedAssociatedTypesWithDefaults

protocol P<Primary> {
  associatedtype Primary: ~Copyable
  associatedtype Secondary: ~Copyable
}

extension P {
  func testCopyability(_ a: Self.Primary,
                       _ b: Self.Secondary) {} // expected-error {{parameter of noncopyable type 'Self.Secondary'}} // expected-note 3{{}}
}

extension P where Self.Primary: ~Copyable {
  func withNoncopyablePrimary(_ a: Self.Primary,      // expected-error {{parameter of noncopyable type 'Self.Primary'}} // expected-note 3{{}}
                              _ b: Self.Secondary) {} // expected-error {{parameter of noncopyable type 'Self.Secondary'}} // expected-note 3{{}}
}


protocol Base<Elm> { associatedtype Elm: ~Copyable } // expected-note {{'Elm' declared here}}
protocol Derived5: Base {
  associatedtype Elm: ~Copyable
  // expected-warning@-1 {{redeclaration of associated type 'Elm' from protocol 'Base' is better expressed as a 'where' clause on the protocol}}

  func check(_: Elm) // expected-error {{parameter of noncopyable type 'Self.Elm' must specify ownership}} // expected-note 3{{}}
}

protocol IterProto<Element>: ~Copyable {
  associatedtype Element: ~Copyable
}

protocol SeqOK {
  associatedtype Element: ~Copyable
  associatedtype Iterator: IterProto where Iterator.Element == Element, Iterator: ~Copyable
}


protocol TestSameTypeInverses<A>: ~Copyable {
  associatedtype A: ~Copyable

  associatedtype One: Iterable where One.Element == A

  associatedtype Two: Iterable

  func check(_ one: One.Element) // expected-error {{parameter of noncopyable type 'Self.A' must specify ownership}} // expected-note 3{{}}
  func check(_ one: Two.Element)
}
protocol Iterable<Element>: ~Copyable {
  associatedtype Element: ~Copyable
}


struct ReqC<T: Copyable> {}

func reqC<T: Copyable>(_ t: T) {} // expected-note 14{{'where T: Copyable' is implicit here}}


protocol ProvideA<A>: ~Copyable {
  associatedtype A: ~Copyable, ProvideB
}

protocol ProvideB: ~Copyable {
  associatedtype B: ~Copyable, ProvideA
}

// via bound-parameter requirement inference, all of these associated types become copyable
func inferenceRec<T: ProvideA>(_ t: T,
                  _ a1:  ReqC<T.A>,
                  _ b2:  ReqC<T.A.B>,
                  _ a3:  ReqC<T.A.B.A>,
                  _ b4:  ReqC<T.A.B.A.B>,
                  _ a5:  ReqC<T.A.B.A.B.A>,
                  _ b6:  ReqC<T.A.B.A.B.A.B>,
                  _ a7:  ReqC<T.A.B.A.B.A.B.A>,
                  _ b8:  ReqC<T.A.B.A.B.A.B.A.B>,
                  _ a9:  ReqC<T.A.B.A.B.A.B.A.B.A>,
                  _ b10: ReqC<T.A.B.A.B.A.B.A.B.A.B>,
                  _ a11: ReqC<T.A.B.A.B.A.B.A.B.A.B.A>,
                  _ b12: ReqC<T.A.B.A.B.A.B.A.B.A.B.A.B>
                  ) {}

// Otherwise, the archetypes infer Copyable based on the defaulting rule,
// no matter the depth.
func inferenceRec<T: ProvideA>(_ t: T,
  _ a1:  borrowing T.A,
  _ b2:  borrowing T.A.B,
  _ a3:  borrowing T.A.B.A,
  _ b4:  borrowing T.A.B.A.B,
  _ a5:  borrowing T.A.B.A.B.A,
  _ b6:  borrowing T.A.B.A.B.A.B,
  _ a7:  borrowing T.A.B.A.B.A.B.A,
  _ b8:  borrowing T.A.B.A.B.A.B.A.B,
  _ a9:  borrowing T.A.B.A.B.A.B.A.B.A,
  _ b10: borrowing T.A.B.A.B.A.B.A.B.A.B,
  _ a11: borrowing T.A.B.A.B.A.B.A.B.A.B.A,
  _ b12: borrowing T.A.B.A.B.A.B.A.B.A.B.A.B
  ) {
  reqC(a1)
  reqC(b2) // expected-error {{requires that 'T.A.B' conform to 'Copyable'}}
  reqC(a3)
  reqC(b4) // expected-error {{requires that 'T.A.B.A.B' conform to 'Copyable'}}
  reqC(a5)
  reqC(b6) // expected-error {{requires that 'T.A.B.A.B.A.B' conform to 'Copyable'}}
  reqC(a7)
  reqC(b8) // expected-error {{requires that 'T.A.B.A.B.A.B.A.B' conform to 'Copyable'}}
  reqC(a9)
  reqC(b10) // expected-error {{requires that 'T.A.B.A.B.A.B.A.B.A.B' conform to 'Copyable'}}
  reqC(a11)
  reqC(b12) // expected-error {{requires that 'T.A.B.A.B.A.B.A.B.A.B.A.B' conform to 'Copyable'}}
}

protocol Gen<E>: ~Copyable {
  associatedtype E: ~Copyable, Gen
  associatedtype I: ~Copyable, Gen

  func e() -> E
  func i() -> I
}

func checkExistential(_ s: any Gen) {
  reqC(s.e())
  reqC(s.e().e())
  reqC(s.e().e().e())
  reqC(s.e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e().e())

  reqC(s.i()) // expected-error {{requires that 'T' conform to 'Copyable'}}
  reqC(s.i().e())
  reqC(s.i().e().i()) // expected-error {{requires that 'T' conform to 'Copyable'}}
  reqC(s.i().e().i().e())
}

func checkExistential2<R: ~Copyable>(_ s: any Gen<R>) {
  reqC(s.e()) // expected-error {{global function 'reqC' requires that 'R' conform to 'Copyable'}}
  reqC(s.e().e()) // expected-error {{value of type 'R' has no member 'e'}}

  reqC(s.i()) // expected-error {{requires that 'T' conform to 'Copyable'}}
  reqC(s.i().e())
  reqC(s.i().e().i()) // expected-error {{requires that 'T' conform to 'Copyable'}}
  reqC(s.i().e().i().e())
}

// Suppression on the constrained existential only goes far down
// as possible to suppress on the generic type parameter itself.
func checkExistential3<R>(_ s: any Gen<R>) where R: Gen, R: ~Copyable, R.E: ~Copyable {
  reqC(s.e()) // expected-error {{global function 'reqC' requires that 'R' conform to 'Copyable'}}
  reqC(s.e().e()) // expected-error {{global function 'reqC' requires that 'R.E' conform to 'Copyable'}}
  reqC(s.e().e().e())
}

protocol Veggie<A> {
  associatedtype A: ~Copyable
  associatedtype NeedsCopyable

  func a() -> A
}
protocol Carrot: Veggie
  where Self.NeedsCopyable: ~Copyable {} // expected-error {{'Self.NeedsCopyable' required to be 'Copyable' but is marked with '~Copyable'}}

protocol CarrotCake: Carrot where Self.A: ~Copyable {} // expected-error {{'Self.A' required to be 'Copyable' but is marked with '~Copyable'}}

func ex1<Cucumber: ~Copyable, Potato>(_ nc: any Veggie<Cucumber>, c: any Veggie<Potato>) {
  reqC(nc.a()) // expected-error {{global function 'reqC' requires that 'Cucumber' conform to 'Copyable'}}
  reqC(c.a())
}

protocol Bird {
  associatedtype Song
}

protocol Eagle: Bird where Self.Song: ~Copyable {}// expected-error {{'Self.Song' required to be 'Copyable' but is marked with '~Copyable'}}

protocol Pushable<Element> {
  associatedtype Element: ~Copyable
}

struct Stack<Scope: Pushable> {}

func push<Val>(_ s: Stack<Val>, _ v: Val)
  where Val.Element: ~Copyable {} // expected-error {{'Val.Element' required to be 'Copyable' but is marked with '~Copyable'}}

protocol Stackable<Element>: Pushable {}

extension Stackable where Element: ~Copyable {} // expected-error {{'Self.Element' required to be 'Copyable' but is marked with '~Copyable'}}
