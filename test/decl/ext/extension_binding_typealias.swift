// RUN: %target-typecheck-verify-swift

// The protocols below are declared in extensions of A, which are bound after
// the extensions that follow the conforming types. Binding those extensions
// looks into the conforming types.

struct S1: A.P { typealias T = Int }
extension S1.T {}

struct S2: A.P { typealias T = U; typealias U = Int }
extension S2.T {}

struct Outer { struct Inner {} }
struct S3: A.P { typealias T = Outer }
extension S3.T.Inner {}

// Extensions of nested types.
protocol Top {}
class Root {}
struct S4: A.P { struct Nested { typealias T = Int } }
extension S4.Nested.T {}

struct S5: A.P { struct Nested { typealias T = Int } }
extension S5.Nested: Top {}
extension S5.Nested.T {}

struct S6: A.P { struct Nested: Top { typealias T = Int } }
extension S6.Nested.T {}

struct S7: A.P { class Nested: Root { typealias T = Int } }
extension S7.Nested.T {}

// The inheritance clause entry is found through a protocol of the enclosing
// type.
protocol R {}
extension R { typealias TopAlias = Top }
struct S8: R, A.P { struct Nested: TopAlias { typealias T = Int } }
extension S8.Nested.T {}

// Entries that resolve early and entries that resolve late.
struct S9: Top, A.P { typealias T = Int }
extension S9.T {}

// The typealias is found through a protocol from a later extension.
struct S10: A.P2 { typealias V = W }
extension S10.V { func viaLateProtocol() {} }

// Conformances implied by the protocol.
struct S11: A.P3 { typealias T = Int }
extension S11.T {}

// Conformances inherited from a superclass.
class Base1: A.P {}
class Mid1: Base1 {}
class Derived1: Mid1 { typealias T = Int }
extension Derived1.T {}

class Base2 {}
extension Base2: A.P {}
class Derived2: Base2 { typealias T = Int }
extension Derived2.T {}

// Generic and conditional conformances.
struct G1<X>: A.P { typealias T = Int }
extension G1.T {}

struct G2<X> { typealias T = Int }
extension G2.T {}
extension G2: A.P where X == Int {} // expected-note {{requirement from conditional conformance of 'G2<String>' to 'A.P'}}

// Redundant conformances are still diagnosed.
struct S12: A.P { typealias T = Int } // expected-note {{'S12' declares conformance to protocol 'P' here}}
extension S12.T {}
extension S12: A.P {} // expected-error {{redundant conformance of 'S12' to protocol 'P'}}

struct S13: A.P, A.P { typealias T = Int } // expected-error {{redundant conformance of 'S13' to protocol 'P'}} expected-note {{'S13' declares conformance to protocol 'P' here}}
extension S13.T {}

struct S14 { typealias T = Int }
extension S14.T {}
extension S14: A.P {} // expected-note {{'S14' declares conformance to protocol 'P' here}}
extension S14: A.P {} // expected-error {{redundant conformance of 'S14' to protocol 'P'}}

class Base3: A.P {}
class Derived3: Base3, A.P { typealias T = Int } // expected-error {{redundant conformance of 'Derived3' to protocol 'P'}} expected-note {{'Derived3' inherits conformance to protocol 'P' from superclass here}}
extension Derived3.T {}

struct S15: Top, A.TopAlias { typealias T = Int } // expected-error {{redundant conformance of 'S15' to protocol 'Top'}} expected-note {{'S15' declares conformance to protocol 'Top' here}}
extension S15.T {}

// Entries that don't resolve are diagnosed once.
struct S16: A.P, Undefined { typealias T = Int } // expected-error {{cannot find type 'Undefined' in scope}}
extension S16.T {}

// The entry names a typealias for such a protocol.
typealias PAlias = A.P
struct S17: PAlias { typealias T = Int }
extension S17.T {}

// Compositions with components that resolve right away.
struct S18: Top & A.P { typealias T = Int }
extension S18.T {}

typealias TopAndP = Top & A.P
typealias TopAndPAlias = TopAndP
struct S19: TopAndPAlias { typealias T = Int }
extension S19.T {}

struct S20: (Top & A.P), @unchecked Sendable & A.P2 { typealias T = Int } // expected-warning {{'@unchecked' conformance to 'A.P2' has no meaning}}
extension S20.T {}

struct S21: Top & A.P, Top { typealias T = Int } // expected-error {{redundant conformance of 'S21' to protocol 'Top'}} expected-note {{'S21' declares conformance to protocol 'Top' here}}
extension S21.T {}

struct S22: Top & Undefined, A.P { typealias T = Int } // expected-error {{cannot find type 'Undefined' in scope}}
extension S22.T {}

struct S23: isolated (Top & A.P) { typealias T = Int }
extension S23.T {}

// Suppressed conformances stay suppressed.
struct NC: ~Copyable, A.P4 { typealias T = Int }
extension NC.T {}

// Entries that don't name a protocol still don't resolve and are diagnosed
// once.
struct N1: A.Missing { typealias T = Int } // expected-error {{'Missing' is not a member type of enum 'extension_binding_typealias.A'}}
extension N1.T {}

enum Other {} // expected-note {{'Other' declared here}}
struct N2: Other.P { typealias T = Int } // expected-error {{'P' is not a member type of enum 'extension_binding_typealias.Other'}}
extension N2.T {}

typealias Fn = () -> Void
struct N3: Fn, A.P { typealias T = Int } // expected-error {{inheritance from non-protocol type 'Fn' (aka '() -> ()')}}
extension N3.T {}

// Entries whose resolution runs into a cycle are diagnosed once.
extension A { typealias N4OwnAlias = N4.Own }
struct N4: A.N4OwnAlias, Top { typealias T = Int; typealias Own = Top } // expected-error {{circular reference}} expected-note {{through reference here}}
extension N4.T {}

extension A { typealias N5OwnAndTop = N5.Own & Top }
struct N5: A.N5OwnAndTop { typealias T = Int; typealias Own = Top } // expected-error {{circular reference}} expected-note {{through reference here}}
extension N5.T {}

// Superclasses don't gain the conformances of their subclasses.
class Base4 {}
class Derived4: Base4, A.P { typealias T = Int }
extension Derived4.T {}

// Types from other modules keep their conformances.
extension Int: A.P {}
extension Int.Stride {}

enum A {} // expected-note {{'A' declared here}}
extension A { protocol P {} }
extension A { protocol P2 {} }
extension A.P2 { typealias W = Int }
extension A { protocol P3: Top {} }
extension A { protocol P4: ~Copyable {} }
extension A { typealias TopAlias = Top }

func takesP(_: any A.P) {}
func takesP2(_: any A.P2) {}
func takesGenericP(_: some A.P) {}
func takesTop(_: any Top) {}
func takesP4<T: A.P4 & ~Copyable>(_: borrowing T) {}
func takesCopyable<T>(_: T) {} // expected-note {{'where T: Copyable' is implicit here}}
func testConformance() {
  takesP(S1())
  takesP(S2())
  takesP(S3())
  takesP(S4())
  takesP(S5())
  takesP(S6())
  takesP(S7())
  takesP(S8())
  takesP(S9())
  takesTop(S9())
  takesP2(S10())
  takesTop(S11())
  takesP(Derived1())
  takesP(Mid1())
  takesP(Derived2())
  takesGenericP(G1<String>())
  takesGenericP(G2<Int>())
  takesP(S16())
  takesP(S17())
  takesP(S18())
  takesTop(S18())
  takesP(S19())
  takesTop(S19())
  takesP(S20())
  takesP2(S20())
  takesP(S21())
  takesP(S22())
  takesTop(S22())
  takesP(S23())
  takesTop(S23())
  takesP(Derived4())
  takesP(0)
  _ = 0 as any BinaryInteger
}

func testNonConformance() {
  takesP4(NC())
  takesCopyable(NC()) // expected-error {{global function 'takesCopyable' requires that 'NC' conform to 'Copyable'}}
  takesP(S4.Nested()) // expected-error {{argument type 'S4.Nested' does not conform to expected type 'A.P'}}
  takesP(Base4()) // expected-error {{argument type 'Base4' does not conform to expected type 'A.P'}}
  takesGenericP(G2<String>()) // expected-error {{global function 'takesGenericP' requires the types 'String' and 'Int' be equivalent}}
}

// Typealiases from protocol extensions are still found through conformances.
protocol Q {}
extension Q { typealias U = Int }

struct S24: Q { typealias V = U }
extension S24.V { func viaConformance() {} }

class Base5: Q {}
class Derived5: Base5 { typealias V = U }
extension Derived5.V { func viaSuperclass() {} }

func testLookup() {
  0.viaConformance()
  0.viaSuperclass()
  0.viaLateProtocol()
}
