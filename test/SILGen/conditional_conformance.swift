// RUN: %target-swift-emit-silgen %s | %FileCheck %s

protocol P1 {
  func normal()
  func generic<T: P3>(_: T)
}
protocol P2 {}
protocol P3 {}

protocol P4 {
  associatedtype AT
}

struct Conformance<A> {}
extension Conformance: P1 where A: P2 {
  func normal() {}
  func generic<T: P3>(_: T) {}
}

// This is defined below but is emitted before any witness tables.
// Just make sure it does not have a generic signature.
//
// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s23conditional_conformance11ConformanceVyxGAA2P1A2A2P2RzlAaEP6normalyyFTW : $@convention(witness_method: P1) <τ_0_0 where τ_0_0 : P2> (@in_guaranteed Conformance<τ_0_0>) -> () {


// CHECK-LABEL: sil_witness_table hidden <A where A : P2> Conformance<A>: P1 module conditional_conformance {
// CHECK-NEXT:    method #P1.normal: <Self where Self : P1> (Self) -> () -> () : @$s23conditional_conformance11ConformanceVyxGAA2P1A2A2P2RzlAaEP6normalyyFTW	// protocol witness for P1.normal() in conformance <A> Conformance<A>
// CHECK-NEXT:    method #P1.generic: <Self where Self : P1><T where T : P3> (Self) -> (T) -> () : @$s23conditional_conformance11ConformanceVyxGAA2P1A2A2P2RzlAaEP7genericyyqd__AA2P3Rd__lFTW	// protocol witness for P1.generic<A>(_:) in conformance <A> Conformance<A>
// CHECK-NEXT:    conditional_conformance (A: P2): dependent
// CHECK-NEXT:  }

struct ConformanceAssoc<A> {}
extension ConformanceAssoc: P1 where A: P4, A.AT: P2 {
  func normal() {}
  func generic<T: P3>(_: T) {}
}
// CHECK-LABEL: sil_witness_table hidden <A where A : P4, A.AT : P2> ConformanceAssoc<A>: P1 module conditional_conformance {
// CHECK-NEXT:    method #P1.normal: <Self where Self : P1> (Self) -> () -> () : @$s23conditional_conformance16ConformanceAssocVyxGAA2P1A2A2P4RzAA2P22ATRpzlAaEP6normalyyFTW // protocol witness for P1.normal() in conformance <A> ConformanceAssoc<A>
// CHECK-NEXT:    method #P1.generic: <Self where Self : P1><T where T : P3> (Self) -> (T) -> () : @$s23conditional_conformance16ConformanceAssocVyxGAA2P1A2A2P4RzAA2P22ATRpzlAaEP7genericyyqd__AA2P3Rd__lFTW // protocol witness for P1.generic<A>(_:) in conformance <A> ConformanceAssoc<A>
// CHECK-NEXT:    conditional_conformance (A: P4): dependent
// CHECK-NEXT:    conditional_conformance (A.AT: P2): dependent
// CHECK-NEXT:  }

struct SameTypeConcrete<B> {}
extension SameTypeConcrete: P1 where B == Int {
  func normal() {}
  func generic<T: P3>(_: T) {}
}

// CHECK-LABEL: sil_witness_table hidden <B where B == Int> SameTypeConcrete<B>: P1 module conditional_conformance {
// CHECK-NEXT:    method #P1.normal: <Self where Self : P1> (Self) -> () -> () : @$s23conditional_conformance16SameTypeConcreteVySiGAA2P1A2aEP6normalyyFTW	// protocol witness for P1.normal() in conformance SameTypeConcrete<Int>
// CHECK-NEXT:    method #P1.generic: <Self where Self : P1><T where T : P3> (Self) -> (T) -> () : @$s23conditional_conformance16SameTypeConcreteVySiGAA2P1A2aEP7genericyyqd__AA2P3Rd__lFTW	// protocol witness for P1.generic<A>(_:) in conformance SameTypeConcrete<Int>
// CHECK-NEXT:  }

struct SameTypeGeneric<C, D> {}
extension SameTypeGeneric: P1 where C == D {
  func normal() {}
  func generic<T: P3>(_: T) {}
}

struct SameTypeGenericConcrete<E, F> {}
extension SameTypeGenericConcrete: P1 where E == [F] {
  func normal() {}
  func generic<T: P3>(_: T) {}
}

struct Everything<G, H, I, J, K, L> {}
extension Everything: P1 where G: P2, H == Int, I == J, K == [L] {
  func normal() {}
  func generic<T: P3>(_: T) {}
}

struct IsP2: P2 {}
struct IsNotP2 {}

class Base<A> {}
extension Base: P1 where A: P2 {
  func normal() {}
  func generic<T: P3>(_: T) {}
}
// CHECK-LABEL: sil_witness_table hidden <A where A : P2> Base<A>: P1 module conditional_conformance {
// CHECK-NEXT:  method #P1.normal: <Self where Self : P1> (Self) -> () -> () : @$s23conditional_conformance4BaseCyxGAA2P1A2A2P2RzlAaEP6normalyyFTW	// protocol witness for P1.normal() in conformance <A> Base<A>
// CHECK-NEXT:    method #P1.generic: <Self where Self : P1><T where T : P3> (Self) -> (T) -> () : @$s23conditional_conformance4BaseCyxGAA2P1A2A2P2RzlAaEP7genericyyqd__AA2P3Rd__lFTW	// protocol witness for P1.generic<A>(_:) in conformance <A> Base<A>
// CHECK-NEXT:    conditional_conformance (A: P2): dependent
// CHECK-NEXT:  }

// These don't get separate witness tables, but shouldn't crash anything.
class SubclassGood: Base<IsP2> {}
class SubclassBad: Base<IsNotP2> {}
