// RUN: %target-typecheck-verify-swift -typecheck -verify
// RUN: %target-typecheck-verify-swift -typecheck -debug-generic-signatures %s > %t.dump 2>&1
// RUN: %FileCheck %s < %t.dump

protocol P1 {}
protocol P2 {}
protocol P3 {}
protocol P4: P1 {}
protocol P5: P2 {}
protocol P6: P2 {}

protocol Assoc { associatedtype AT }

func takes_P2<X: P2>(_: X) {}
// FIXME: "requirement specified as..." isn't accurate below
// expected-note@-2{{candidate requires that the types 'U' and 'V' be equivalent (requirement specified as 'U' == 'V')}}
// expected-note@-3{{requirement from conditional conformance of 'SameTypeGeneric<U, V>' to 'P2'}}
// expected-note@-4{{candidate requires that the types 'U' and 'Int' be equivalent (requirement specified as 'U' == 'Int')}}
// expected-note@-5{{requirement from conditional conformance of 'SameTypeGeneric<U, Int>' to 'P2'}}
// expected-note@-6{{candidate requires that the types 'Int' and 'Float' be equivalent (requirement specified as 'Int' == 'Float')}}
// expected-note@-7{{requirement from conditional conformance of 'SameTypeGeneric<Int, Float>' to 'P2'}}
// expected-note@-8{{candidate requires that 'C1' inherit from 'U' (requirement specified as 'U' : 'C1')}}
// expected-note@-9{{requirement from conditional conformance of 'ClassFree<U>' to 'P2'}}
// expected-note@-10{{candidate requires that 'C3' inherit from 'U' (requirement specified as 'U' : 'C3')}}
// expected-note@-11{{requirement from conditional conformance of 'ClassMoreSpecific<U>' to 'P2'}}
// expected-note@-12{{candidate requires that 'C1' inherit from 'Int' (requirement specified as 'Int' : 'C1')}}
// expected-note@-13{{requirement from conditional conformance of 'SubclassBad' to 'P2'}}
// expected-note@-14{{candidate requires that the types 'Float' and 'Int' be equivalent (requirement specified as 'Float' == 'Int')}}
// expected-note@-15{{requirement from conditional conformance of 'Infer<Constrained<U>, V>' to 'P2'}}
// expected-note@-16{{candidate requires that the types 'Constrained<U>' and 'Constrained<V>' be equivalent (requirement specified as 'Constrained<U>' == 'Constrained<V>')}}
// expected-note@-17{{candidate requires that the types 'U' and 'Int' be equivalent (requirement specified as 'U' == 'Int')}}
// expected-note@-18{{requirement from conditional conformance of 'SameType<Float>' to 'P2'}}
// expected-note@-19{{requirement from conditional conformance of 'SameType<U>' to 'P2'}}
func takes_P5<X: P5>(_: X) {}

struct Free<T> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=Free<T>
// CHECK-NEXT: (normal_conformance type=Free<T> protocol=P2
// CHECK-NEXT:   conforms_to: T P1)
extension Free: P2 where T: P1 {}
func free_good<U: P1>(_: U) {
    takes_P2(Free<U>())
}
func free_bad<U>(_: U) {
    takes_P2(Free<U>()) // expected-error{{type 'U' does not conform to protocol 'P1'}}}}
    // expected-error@-1{{'<X where X : P2> (X) -> ()' requires that 'U' conform to 'P1'}}
    // expected-note@-2{{requirement specified as 'U' : 'P1'}}
    // expected-note@-3{{requirement from conditional conformance of 'Free<U>' to 'P2'}}
}

struct Constrained<T: P1> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=Constrained<T>
// CHECK-NEXT: (normal_conformance type=Constrained<T> protocol=P2
// CHECK-NEXT:   conforms_to: T P3)
extension Constrained: P2 where T: P3 {}
func constrained_good<U: P1 & P3>(_: U) {
    takes_P2(Constrained<U>())
}
func constrained_bad<U: P1>(_: U) {
    takes_P2(Constrained<U>()) // expected-error{{type 'U' does not conform to protocol 'P3'}}
    // expected-error@-1{{'<X where X : P2> (X) -> ()' requires that 'U' conform to 'P3'}}
    // expected-note@-2{{requirement specified as 'U' : 'P3'}}
    // expected-note@-3{{requirement from conditional conformance of 'Constrained<U>' to 'P2'}}
}

struct RedundantSame<T: P1> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=RedundantSame<T>
// CHECK-NEXT: (normal_conformance type=RedundantSame<T> protocol=P2)
extension RedundantSame: P2 where T: P1 {}

struct RedundantSuper<T: P4> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=RedundantSuper<T>
// CHECK-NEXT: (normal_conformance type=RedundantSuper<T> protocol=P2)
extension RedundantSuper: P2 where T: P1 {}

struct OverlappingSub<T: P1> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=OverlappingSub<T>
// CHECK-NEXT: (normal_conformance type=OverlappingSub<T> protocol=P2
// CHECK-NEXT:   conforms_to: T P4)
extension OverlappingSub: P2 where T: P4 {}
func overlapping_sub_good<U: P4>(_: U) {
    takes_P2(OverlappingSub<U>())
}
func overlapping_sub_bad<U: P1>(_: U) {
    takes_P2(OverlappingSub<U>()) // expected-error{{type 'U' does not conform to protocol 'P4'}}
    // expected-error@-1{{'<X where X : P2> (X) -> ()' requires that 'U' conform to 'P4'}}
    // expected-note@-2{{requirement specified as 'U' : 'P4'}}
    // expected-note@-3{{requirement from conditional conformance of 'OverlappingSub<U>' to 'P2'}}
}


struct SameType<T> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=SameType<Int>
// CHECK-NEXT: (normal_conformance type=SameType<T> protocol=P2
// CHECK-NEXT:   same_type: T Int)
extension SameType: P2 where T == Int {}
func same_type_good() {
    takes_P2(SameType<Int>())
}
func same_type_bad<U>(_: U) {
    takes_P2(SameType<U>()) // expected-error{{cannot invoke 'takes_P2(_:)' with an argument list of type '(SameType<U>)'}}
    takes_P2(SameType<Float>()) // expected-error{{cannot invoke 'takes_P2(_:)' with an argument list of type '(SameType<Float>)'}}
}


struct SameTypeGeneric<T, U> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=SameTypeGeneric<T, T>
// CHECK-NEXT: (normal_conformance type=SameTypeGeneric<T, U> protocol=P2
// CHECK-NEXT:   same_type: T U)
extension SameTypeGeneric: P2 where T == U {}
func same_type_generic_good<U, V>(_: U, _: V)
  where U: Assoc, V: Assoc, U.AT == V.AT
{
    takes_P2(SameTypeGeneric<Int, Int>())
    takes_P2(SameTypeGeneric<U, U>())
    takes_P2(SameTypeGeneric<U.AT, V.AT>())
}
func same_type_bad<U, V>(_: U, _: V) {
    takes_P2(SameTypeGeneric<U, V>())
    // expected-error@-1{{cannot invoke 'takes_P2(_:)' with an argument list of type '(SameTypeGeneric<U, V>)'}}
    takes_P2(SameTypeGeneric<U, Int>())
    // expected-error@-1{{cannot invoke 'takes_P2(_:)' with an argument list of type '(SameTypeGeneric<U, Int>)'}}
    takes_P2(SameTypeGeneric<Int, Float>())
    // expected-error@-1{{cannot invoke 'takes_P2(_:)' with an argument list of type '(SameTypeGeneric<Int, Float>)'}}
}


struct Infer<T, U> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=Infer<Constrained<U>, U>
// CHECK-NEXT: (normal_conformance type=Infer<T, U> protocol=P2
// CHECK-NEXT:   same_type: T Constrained<U>
// CHECK-NEXT:   conforms_to:  U P1)
extension Infer: P2 where T == Constrained<U> {}
func infer_good<U: P1>(_: U) {
    takes_P2(Infer<Constrained<U>, U>())
}
func infer_bad<U: P1, V>(_: U, _: V) {
    takes_P2(Infer<Constrained<U>, V>())
    // expected-error@-1{{cannot invoke 'takes_P2(_:)' with an argument list of type '(Infer<Constrained<U>, V>)'}}
    takes_P2(Infer<Constrained<V>, V>())
    // expected-error@-1{{type 'V' does not conform to protocol 'P1'}}
}

struct InferRedundant<T, U: P1> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=InferRedundant<Constrained<U>, U>
// CHECK-NEXT: (normal_conformance type=InferRedundant<T, U> protocol=P2
// CHECK-NEXT:   same_type: T Constrained<U>)
extension InferRedundant: P2 where T == Constrained<U> {}
func infer_redundant_good<U: P1>(_: U) {
    takes_P2(InferRedundant<Constrained<U>, U>())
}
func infer_redundant_bad<U: P1, V>(_: U, _: V) {
  takes_P2(InferRedundant<Constrained<U>, V>())
  // expected-error@-1{{type 'V' does not conform to protocol 'P1'}}
  takes_P2(InferRedundant<Constrained<V>, V>())
  // expected-error@-1{{type 'V' does not conform to protocol 'P1'}}
}


class C1 {}
class C2: C1 {}
class C3: C2 {}

struct ClassFree<T> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=ClassFree<T>
// CHECK-NEXT: (normal_conformance type=ClassFree<T> protocol=P2
// CHECK-NEXT:   superclass: T C1)
extension ClassFree: P2 where T: C1 {}
func class_free_good<U: C1>(_: U) {
    takes_P2(ClassFree<U>())
}
func class_free_bad<U>(_: U) {
    takes_P2(ClassFree<U>())
    // expected-error@-1{{cannot invoke 'takes_P2(_:)' with an argument list of type '(ClassFree<U>)'}}
}

struct ClassMoreSpecific<T: C1> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=ClassMoreSpecific<T>
// CHECK-NEXT: (normal_conformance type=ClassMoreSpecific<T> protocol=P2
// CHECK-NEXT:   superclass: T C3)
extension ClassMoreSpecific: P2 where T: C3 {}
func class_more_specific_good<U: C3>(_: U) {
    takes_P2(ClassMoreSpecific<U>())
}
func class_more_specific_bad<U: C1>(_: U) {
    takes_P2(ClassMoreSpecific<U>())
    // expected-error@-1{{cannot invoke 'takes_P2(_:)' with an argument list of type '(ClassMoreSpecific<U>)'}}
}


struct ClassLessSpecific<T: C3> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=ClassLessSpecific<T>
// CHECK-NEXT: (normal_conformance type=ClassLessSpecific<T> protocol=P2)
extension ClassLessSpecific: P2 where T: C1 {}


// Inherited conformances:
class Base<T> {}
extension Base: P2 where T: C1 {}

class SubclassGood: Base<C1> {}
func subclass_good() {
  takes_P2(SubclassGood())
}
class SubclassBad: Base<Int> {}
func subclass_bad() {
  takes_P2(SubclassBad())
  // expected-error@-1{{cannot invoke 'takes_P2(_:)' with an argument list of type '(SubclassBad)'}}
}

// Inheriting conformances:

struct InheritEqual<T> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=InheritEqual<T>
// CHECK-NEXT:  (normal_conformance type=InheritEqual<T> protocol=P2
// CHECK-NEXT:    conforms_to: T P1)
extension InheritEqual: P2 where T: P1 {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=InheritEqual<T>
// CHECK-NEXT:  (normal_conformance type=InheritEqual<T> protocol=P5
// CHECK-NEXT:    conforms_to: T P1)
extension InheritEqual: P5 where T: P1 {}
func inheritequal_good<U: P1>(_: U) {
  takes_P2(InheritEqual<U>())
  takes_P5(InheritEqual<U>())
}
func inheritequal_bad<U>(_: U) {
  takes_P2(InheritEqual<U>()) // expected-error{{type 'U' does not conform to protocol 'P1'}}
  // expected-error@-1{{'<X where X : P2> (X) -> ()' requires that 'U' conform to 'P1'}}
  // expected-note@-2{{requirement specified as 'U' : 'P1'}}
  // expected-note@-3{{requirement from conditional conformance of 'InheritEqual<U>' to 'P2'}}
  takes_P5(InheritEqual<U>()) // expected-error{{type 'U' does not conform to protocol 'P1'}}
  // expected-error@-1{{'<X where X : P5> (X) -> ()' requires that 'U' conform to 'P1'}}
  // expected-note@-2{{requirement specified as 'U' : 'P1'}}
  // expected-note@-3{{requirement from conditional conformance of 'InheritEqual<U>' to 'P5'}}
}

struct InheritLess<T> {}
extension InheritLess: P2 where T: P1 {}
extension InheritLess: P5 {} // expected-error{{type 'T' does not conform to protocol 'P1'}}
// expected-error@-1{{'P5' requires that 'T' conform to 'P1'}}
// expected-note@-2{{requirement specified as 'T' : 'P1'}}
// expected-note@-3{{requirement from conditional conformance of 'InheritLess<T>' to 'P2'}}


struct InheritMore<T> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=InheritMore<T>
// CHECK-NEXT:  (normal_conformance type=InheritMore<T> protocol=P2
// CHECK-NEXT:    conforms_to: T P1)
extension InheritMore: P2 where T: P1 {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=InheritMore<T>
// CHECK-NEXT:  (normal_conformance type=InheritMore<T> protocol=P5
// CHECK-NEXT:    conforms_to: T P4)
extension InheritMore: P5 where T: P4 {}
func inheritequal_good_good<U: P4>(_: U) {
  takes_P2(InheritMore<U>())
  takes_P5(InheritMore<U>())
}
func inheritequal_good_bad<U: P1>(_: U) {
  takes_P2(InheritMore<U>())
  takes_P5(InheritMore<U>()) // expected-error{{type 'U' does not conform to protocol 'P4'}}
  // expected-error@-1{{'<X where X : P5> (X) -> ()' requires that 'U' conform to 'P4'}}
  // expected-note@-2{{requirement specified as 'U' : 'P4'}}
  // expected-note@-3{{requirement from conditional conformance of 'InheritMore<U>' to 'P5'}}
}
func inheritequal_bad_bad<U>(_: U) {
  takes_P2(InheritMore<U>()) // expected-error{{type 'U' does not conform to protocol 'P1'}}
  // expected-error@-1{{'<X where X : P2> (X) -> ()' requires that 'U' conform to 'P1'}}
  // expected-note@-2{{requirement specified as 'U' : 'P1'}}
  // expected-note@-3{{requirement from conditional conformance of 'InheritMore<U>' to 'P2'}}
  takes_P5(InheritMore<U>()) // expected-error{{type 'U' does not conform to protocol 'P4'}}
  // expected-error@-1{{'<X where X : P5> (X) -> ()' requires that 'U' conform to 'P4'}}
  // expected-note@-2{{requirement specified as 'U' : 'P4'}}
  // expected-note@-3{{requirement from conditional conformance of 'InheritMore<U>' to 'P5'}}
}

struct InheritImplicitOne<T> {}
// This shouldn't give anything implicit since we disallow implication for
// conditional conformances (in many cases, the implied bounds are
// incorrect/insufficiently general).
extension InheritImplicitOne: P5 where T: P1 {}
// expected-error@-1{{conditional conformance of type 'InheritImplicitOne<T>' to protocol 'P5' does not imply conformance to inherited protocol 'P2'}}
// expected-note@-2{{did you mean to explicitly state the conformance like 'extension InheritImplicitOne: P2 where ...'?}}

struct InheritImplicitTwo<T> {}
// Even if we relax the rule about implication, this double-up should still be
// an error, because either conformance could imply InheritImplicitTwo: P2.
extension InheritImplicitTwo: P5 where T: P1 {}
// expected-error@-1{{conditional conformance of type 'InheritImplicitTwo<T>' to protocol 'P5' does not imply conformance to inherited protocol 'P2'}}
// expected-note@-2{{did you mean to explicitly state the conformance like 'extension InheritImplicitTwo: P2 where ...'?}}
extension InheritImplicitTwo: P6 where T: P1 {}

// However, if there's a non-conditional conformance that implies something, we
// can imply from that one.
struct InheritImplicitGood1<T> {}
extension InheritImplicitGood1: P5 {}
extension InheritImplicitGood1: P6 where T: P1 {}

func inheritimplicitgood1<T>(_ : T) {
    takes_P2(InheritImplicitGood1<T>()) // unconstrained!
    takes_P2(InheritImplicitGood1<Int>())
}
struct InheritImplicitGood2<T> {}
extension InheritImplicitGood2: P6 where T: P1 {}
extension InheritImplicitGood2: P5 {}

func inheritimplicitgood2<T>(_: T) {
    takes_P2(InheritImplicitGood2<T>()) // unconstrained!
    takes_P2(InheritImplicitGood2<Int>())

}
struct InheritImplicitGood3<T>: P5 {}
extension InheritImplicitGood3: P6 where T: P1 {}

func inheritimplicitgood3<T>(_: T) {
    takes_P2(InheritImplicitGood3<T>()) // unconstrained!
    takes_P2(InheritImplicitGood3<Int>())
}

// "Multiple conformances" from SE0143

struct TwoConformances<T> {}
extension TwoConformances: P2 where T: P1 {}
// expected-note@-1{{'TwoConformances<T>' declares conformance to protocol 'P2' here}}
extension TwoConformances: P2 where T: P3 {}
// expected-error@-1{{redundant conformance of 'TwoConformances<T>' to protocol 'P2'}}

struct TwoDisjointConformances<T> {}
extension TwoDisjointConformances: P2 where T == Int {}
// expected-note@-1{{'TwoDisjointConformances<T>' declares conformance to protocol 'P2' here}}
extension TwoDisjointConformances: P2 where T == String {}
// expected-error@-1{{redundant conformance of 'TwoDisjointConformances<T>' to protocol 'P2'}}


// FIXME: these cases should be equivalent (and both with the same output as the
// first), but the second one choses T as the representative of the
// equivalence class containing both T and U in the extension's generic
// signature, meaning the stored conditional requirement is T: P1, which isn't
// true in the original type's generic signature.
struct RedundancyOrderDependenceGood<T: P1, U> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=RedundancyOrderDependenceGood<T, T>
// CHECK-NEXT: (normal_conformance type=RedundancyOrderDependenceGood<T, U> protocol=P2
// CHECK-NEXT:   same_type: T U)
extension RedundancyOrderDependenceGood: P2 where U: P1, T == U {}
struct RedundancyOrderDependenceBad<T, U: P1> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=RedundancyOrderDependenceBad<T, T>
// CHECK-NEXT: (normal_conformance type=RedundancyOrderDependenceBad<T, U> protocol=P2
// CHECK-NEXT:   conforms_to: T P1
// CHECK-NEXT:   same_type: T U)
extension RedundancyOrderDependenceBad: P2 where T: P1, T == U {}

// Checking of conditional requirements for existential conversions.
func existential_good<T: P1>(_: T.Type) {
  _ = Free<T>() as P2
}

func existential_bad<T>(_: T.Type) {
  // FIXME: Poor diagnostic.
  _ = Free<T>() as P2 // expected-error{{'Free<T>' is not convertible to 'P2'; did you mean to use 'as!' to force downcast?}}
}

// rdar://problem/35837054
protocol P7 { }

protocol P8 {
  associatedtype A
}

struct X0 { }

struct X1 { }

extension X1: P8 {
  typealias A = X0
}

struct X2<T> { }

extension X2: P7 where T: P8, T.A: P7 { }

func takesF7<T: P7>(_: T) { }
func passesConditionallyNotF7(x21: X2<X1>) {
  takesF7(x21) // expected-error{{type 'X1.A' (aka 'X0') does not conform to protocol 'P7'}}
  // expected-error@-1{{'<T where T : P7> (T) -> ()' requires that 'X1.A' (aka 'X0') conform to 'P7'}}
  // expected-note@-2{{requirement specified as 'X1.A' (aka 'X0') : 'P7'}}
  // expected-note@-3{{requirement from conditional conformance of 'X2<X1>' to 'P7'}}
}


public struct SR6990<T, U> {}
extension SR6990: Sequence where T == Int {
    public typealias Element = Float
    public typealias Iterator = IndexingIterator<[Float]>
    public func makeIterator() -> Iterator { fatalError() }
}
