// RUN: %target-typecheck-verify-swift

// The protocols below inherit from protocols declared in extensions of A,
// which are bound after the extensions that follow the conforming types.
// Binding those extensions looks into the conforming types.

protocol R1: A.P {}
struct S1: R1 { typealias T = Int }
extension S1.T {}

// Protocols that inherit from such protocols.
protocol R2: R1 {}
struct S2: R2 { typealias T = Int }
extension S2.T {}

// The protocol itself triggers the binding.
protocol R3: A.P { typealias T = Int }
extension R3.T {}
struct S3: R3 {}

// Protocols named through a typealias, and a protocol with requirements.
protocol R4: A.QAlias, A.P {}
struct S4: R4 { typealias T = Int; func q() {} }
extension S4.T {}

// A protocol that inherits from such a protocol in its where clause.
protocol R5 where Self: A.P {}
struct S5: R5 { typealias T = Int }
extension S5.T {}

// Protocols that name such a protocol through a typealias.
typealias PAlias = A.P
protocol R6: PAlias {}
struct S6: R6 { typealias T = Int }
extension S6.T {}

protocol R7 where Self: PAlias {}
struct S7: R7 { typealias T = Int }
extension S7.T {}

// Compositions with components that resolve right away.
protocol Top {}
protocol R8: Top & A.P {}
struct S8: R8 { typealias T = Int }
extension S8.T {}

protocol R9 where Self: Top & PAlias {}
struct S9: R9 { typealias T = Int }
extension S9.T {}

protocol R10: isolated (Top & A.P) {}
struct S10: R10 { typealias T = Int }
extension S10.T {}

// Entries that don't resolve and cycles are diagnosed.
protocol N1: A.Missing { typealias T = Int } // expected-error {{'Missing' is not a member type of enum 'extension_binding_inherited_protocol_typealias.A'}}
extension N1.T {}

protocol N2: A.N3 { typealias T = Int } // expected-note {{through protocol 'N2' declared here}}
extension N2.T {}

enum A {} // expected-note {{'A' declared here}}
extension A {
  protocol P {}
  protocol Q { func q() }
  typealias QAlias = Q
  protocol N3: N2 {} // expected-error {{protocol 'N3' refines itself}}
}

func takesP(_: any A.P) {}
func takesQ(_: some A.Q) {}
func generic<T: R2>(_ t: T) { takesP(t) }

func testConformance() {
  takesP(S1())
  takesP(S2())
  takesP(S3())
  takesP(S4())
  takesQ(S4())
  takesP(S5())
  takesP(S6())
  takesP(S7())
  takesP(S8())
  takesP(S9())
  takesP(S10())
  generic(S2())
}
