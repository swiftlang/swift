// RUN: %target-typecheck-verify-swift

// The superclasses below are declared in extensions of A, or named through
// typealiases in them, which are bound after the extensions that follow the
// subclasses. Binding those extensions looks into the subclasses.

protocol Top {}
class Base: Top { func base() {} }
class GenericBase<T>: Top {}
protocol Other {}

class C1: A.BaseAlias { typealias T = Int; override func base() {} }
extension C1.T {}

// Subclasses of such classes.
class C2: C1 { typealias T = Int }
extension C2.T {}

// A generic superclass.
class C3: A.GenericBaseAlias<String> { typealias T = Int }
extension C3.T {}

// A superclass and a protocol named through typealiases from the same
// extension.
class C4: A.BaseAlias, A.OtherAlias { typealias T = Int }
extension C4.T {}

// Protocols with a superclass bound.
protocol P1: A.BaseAlias { typealias T = Int }
extension P1.T {}
final class C5: Base, P1 {}

// A superclass declared in an extension.
class C6: A.NestedBase { typealias T = Int }
extension C6.T {}

// The entry names a typealias for a typealias in such an extension.
typealias TopBaseAlias = A.BaseAlias
class C7: TopBaseAlias { typealias T = Int; override func base() {} }
extension C7.T {}

// Protocols with a superclass bound in their where clause.
protocol P2 where Self: A.BaseAlias { typealias T = Int }
extension P2.T {}
final class C8: Base, P2 {}

// Compositions with components that resolve right away.
class C9: A.BaseAlias & Other { typealias T = Int }
extension C9.T {}

protocol P3 where Self: A.BaseAlias & Other { typealias T = Int }
extension P3.T {}
final class C10: Base, Other, P3 {}

class C11: isolated (A.BaseAlias & Other) { typealias T = Int }
extension C11.T {}

// Entries that don't resolve, cycles and final classes are diagnosed.
class N1: A.Missing { typealias T = Int } // expected-error {{'Missing' is not a member type of enum 'extension_binding_superclass_typealias.A'}}
extension N1.T {}

class N2: A.N3Alias { typealias T = Int } // expected-error {{'N2' inherits from itself}}
class N3: N2 {} // expected-note {{through class 'N3' declared here}}
extension N2.T {}

final class Final {}
class N4: A.FinalAlias { typealias T = Int } // expected-error {{inheritance from a final class 'Final'}}
extension N4.T {}

// Typealiases for a member of the class or protocol, or of a subclass.
class N5: A.N5InnerAlias { class Inner {}; typealias T = Int } // expected-error {{'N5' inherits from itself}}
extension N5.T {}

class N6: A.N7InnerAlias { typealias T = Int } // expected-error {{'N6' inherits from itself}}
class N7: N6 { class Inner {} } // expected-note {{through class 'N7' declared here}}
extension N6.T {}

protocol N8 where Self: A.N8OwnAlias { typealias T = Int; typealias Own = Base } // expected-error {{'N8' inherits from itself}} expected-note {{through reference here}}
extension N8.T {}

// A cycle through a subclass whose superclass is computed first.
class N9: A.N10Alias { typealias T = Int } // expected-error {{'N9' inherits from itself}}
class N10: N9 { typealias U = Int } // expected-note {{through class 'N10' declared here}}
extension N10.U {}
extension N9.T {}

// Recomputing an unresolved entry must not diagnose an earlier cycle again.
extension A { typealias N11OwnAlias = N11.Own } // expected-error {{circular reference}} expected-note {{through reference here}}
protocol N11: A.N11OwnAlias, A.OtherAlias { typealias T = Int; typealias Own = Base } // expected-error {{'N11' inherits from itself}} expected-note {{through reference here}}
extension N11.T {}

enum A {} // expected-note {{'A' declared here}}
extension A {
  typealias BaseAlias = Base
  typealias GenericBaseAlias<T> = GenericBase<T>
  typealias OtherAlias = Other
  typealias N3Alias = N3
  typealias FinalAlias = Final
  typealias N5InnerAlias = N5.Inner // expected-note {{through reference here}}
  typealias N7InnerAlias = N7.Inner // expected-note {{through reference here}}
  typealias N8OwnAlias = N8.Own // expected-error {{circular reference}} expected-note {{through reference here}}
  typealias N10Alias = N10
  class NestedBase {}
}

func takesTop(_: any Top) {}
func takesBase(_: Base) {}
func takesOther(_: any Other) {}
func takesP1(_: some P1) {}
func takesGenericBase(_: GenericBase<String>) {}
func takesNestedBase(_: A.NestedBase) {}

func testConformance() {
  takesTop(C1())
  takesBase(C1())
  takesTop(C2())
  takesBase(C2())
  takesTop(C3())
  takesGenericBase(C3())
  takesBase(C4())
  takesOther(C4())
  takesP1(C5())
  C5().base()
  (C5() as any P1).base()
  takesNestedBase(C6())
  takesBase(C7())
  (C8() as any P2).base()
  takesTop(C9())
  takesBase(C9())
  takesOther(C9())
  (C10() as any P3).base()
  takesTop(C11())
  takesBase(C11())
  takesOther(C11())
}
