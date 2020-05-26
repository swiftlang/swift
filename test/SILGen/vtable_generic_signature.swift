// RUN: %target-swift-emit-silgen %s | %FileCheck %s
// RUN: %target-swift-emit-ir %s

protocol P {}
protocol Q : P {}

class ConcreteBase {
  func f<U : Q>(_: U) {}
}

class ConcreteDerivedFromConcreteBase : ConcreteBase {
  override func f<U : P>(_: U) {}
}

class GenericDerivedFromConcreteBase<T> : ConcreteBase {
  override func f<U : P>(_: U) {}
}

class GenericBase<T> {
  func f<U : Q>(_: U) {}
}

class ConcreteDerivedFromGenericBase : GenericBase<Int> {
  override func f<U : P>(_: U) {}
}

class GenericDerivedFromGenericBase<T> : GenericBase<(T) -> Int> {
  override func f<U : P>(_: U) {}
}

// Make sure we call these methods with the correct substitution map.
func call<T, U : P>(_ t: T, _ u: U) {
  ConcreteDerivedFromConcreteBase().f(u)
  GenericDerivedFromConcreteBase<T>().f(u)

  ConcreteDerivedFromGenericBase().f(u)
  GenericDerivedFromGenericBase<T>().f(u)
}

// All the vtable thunks should traffic in <U where U : Q>, because that's
// what the base method declares.

// CHECK-LABEL: sil private [thunk] [ossa] @$s24vtable_generic_signature019ConcreteDerivedFromD4BaseC1fyyxAA1PRzlFAA0dG0CADyyxAA1QRzlFTV : $@convention(method) <τ_0_0 where τ_0_0 : Q> (@in_guaranteed τ_0_0, @guaranteed ConcreteDerivedFromConcreteBase) -> ()
// CHECK-LABEL: sil private [thunk] [ossa] @$s24vtable_generic_signature30GenericDerivedFromConcreteBaseC1fyyqd__AA1PRd__lFAA0gH0CADyyxAA1QRzlFTV : $@convention(method) <τ_0_0><τ_1_0 where τ_1_0 : Q> (@in_guaranteed τ_1_0, @guaranteed GenericDerivedFromConcreteBase<τ_0_0>) -> ()
// CHECK-LABEL: sil private [thunk] [ossa] @$s24vtable_generic_signature30ConcreteDerivedFromGenericBaseC1fyyxAA1PRzlFAA0gH0CADyyqd__AA1QRd__lFTV : $@convention(method) <τ_0_0 where τ_0_0 : Q> (@in_guaranteed τ_0_0, @guaranteed ConcreteDerivedFromGenericBase) -> ()
// CHECK-LABEL: sil private [thunk] [ossa] @$s24vtable_generic_signature018GenericDerivedFromD4BaseC1fyyqd__AA1PRd__lFAA0dG0CADyyqd__AA1QRd__lFTV : $@convention(method) <τ_0_0><τ_1_0 where τ_1_0 : Q> (@in_guaranteed τ_1_0, @guaranteed GenericDerivedFromGenericBase<τ_0_0>) -> ()

// CHECK-LABEL: sil_vtable ConcreteBase {
// CHECK-NEXT:   #ConcreteBase.f: <U where U : Q> (ConcreteBase) -> (U) -> () : @$s24vtable_generic_signature12ConcreteBaseC1fyyxAA1QRzlF
// CHECK-NEXT:   #ConcreteBase.init!allocator: (ConcreteBase.Type) -> () -> ConcreteBase : @$s24vtable_generic_signature12ConcreteBaseCACycfC
// CHECK-NEXT:   #ConcreteBase.deinit!deallocator: @$s24vtable_generic_signature12ConcreteBaseCfD
// CHECK-NEXT: }

// CHECK-LABEL: sil_vtable ConcreteDerivedFromConcreteBase {
// CHECK-NEXT:   #ConcreteBase.f: <U where U : Q> (ConcreteBase) -> (U) -> () : @$s24vtable_generic_signature019ConcreteDerivedFromD4BaseC1fyyxAA1PRzlFAA0dG0CADyyxAA1QRzlFTV [override]
// CHECK-NEXT:   #ConcreteBase.init!allocator: (ConcreteBase.Type) -> () -> ConcreteBase : @$s24vtable_generic_signature019ConcreteDerivedFromD4BaseCACycfC [override]
// CHECK-NEXT:   #ConcreteDerivedFromConcreteBase.f: <U where U : P> (ConcreteDerivedFromConcreteBase) -> (U) -> () : @$s24vtable_generic_signature019ConcreteDerivedFromD4BaseC1fyyxAA1PRzlF
// CHECK-NEXT:   #ConcreteDerivedFromConcreteBase.deinit!deallocator: @$s24vtable_generic_signature019ConcreteDerivedFromD4BaseCfD
// CHECK-NEXT: }

// CHECK-LABEL: sil_vtable GenericDerivedFromConcreteBase {
// CHECK-NEXT:   #ConcreteBase.f: <U where U : Q> (ConcreteBase) -> (U) -> () : @$s24vtable_generic_signature30GenericDerivedFromConcreteBaseC1fyyqd__AA1PRd__lFAA0gH0CADyyxAA1QRzlFTV [override]
// CHECK-NEXT:   #ConcreteBase.init!allocator: (ConcreteBase.Type) -> () -> ConcreteBase : @$s24vtable_generic_signature30GenericDerivedFromConcreteBaseCACyxGycfC [override]
// CHECK-NEXT:   #GenericDerivedFromConcreteBase.f: <T><U where U : P> (GenericDerivedFromConcreteBase<T>) -> (U) -> () : @$s24vtable_generic_signature30GenericDerivedFromConcreteBaseC1fyyqd__AA1PRd__lF
// CHECK-NEXT:   #GenericDerivedFromConcreteBase.deinit!deallocator: @$s24vtable_generic_signature30GenericDerivedFromConcreteBaseCfD
// CHECK-NEXT: }

// CHECK-LABEL: sil_vtable GenericBase {
// CHECK-NEXT:   #GenericBase.f: <T><U where U : Q> (GenericBase<T>) -> (U) -> () : @$s24vtable_generic_signature11GenericBaseC1fyyqd__AA1QRd__lF
// CHECK-NEXT:   #GenericBase.init!allocator: <T> (GenericBase<T>.Type) -> () -> GenericBase<T> : @$s24vtable_generic_signature11GenericBaseCACyxGycfC
// CHECK-NEXT:   #GenericBase.deinit!deallocator: @$s24vtable_generic_signature11GenericBaseCfD
// CHECK-NEXT: }

// CHECK-LABEL: sil_vtable ConcreteDerivedFromGenericBase {
// CHECK-NEXT:   #GenericBase.f: <T><U where U : Q> (GenericBase<T>) -> (U) -> () : @$s24vtable_generic_signature30ConcreteDerivedFromGenericBaseC1fyyxAA1PRzlFAA0gH0CADyyqd__AA1QRd__lFTV [override]
// CHECK-NEXT:   #GenericBase.init!allocator: <T> (GenericBase<T>.Type) -> () -> GenericBase<T> : @$s24vtable_generic_signature30ConcreteDerivedFromGenericBaseCACycfC [override]
// CHECK-NEXT:   #ConcreteDerivedFromGenericBase.f: <U where U : P> (ConcreteDerivedFromGenericBase) -> (U) -> () : @$s24vtable_generic_signature30ConcreteDerivedFromGenericBaseC1fyyxAA1PRzlF
// CHECK-NEXT:   #ConcreteDerivedFromGenericBase.deinit!deallocator: @$s24vtable_generic_signature30ConcreteDerivedFromGenericBaseCfD
// CHECK-NEXT: }

// CHECK-LABEL: sil_vtable GenericDerivedFromGenericBase {
// CHECK-NEXT:   #GenericBase.f: <T><U where U : Q> (GenericBase<T>) -> (U) -> () : @$s24vtable_generic_signature018GenericDerivedFromD4BaseC1fyyqd__AA1PRd__lFAA0dG0CADyyqd__AA1QRd__lFTV [override]
// CHECK-NEXT:   #GenericBase.init!allocator: <T> (GenericBase<T>.Type) -> () -> GenericBase<T> : @$s24vtable_generic_signature018GenericDerivedFromD4BaseCACyxGycfC [override]
// CHECK-NEXT:   #GenericDerivedFromGenericBase.f: <T><U where U : P> (GenericDerivedFromGenericBase<T>) -> (U) -> () : @$s24vtable_generic_signature018GenericDerivedFromD4BaseC1fyyqd__AA1PRd__lF
// CHECK-NEXT:   #GenericDerivedFromGenericBase.deinit!deallocator: @$s24vtable_generic_signature018GenericDerivedFromD4BaseCfD
// CHECK-NEXT: }
