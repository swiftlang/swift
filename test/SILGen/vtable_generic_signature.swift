// RUN: %target-swift-emit-silgen %s | %FileCheck %s

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

// All the vtable thunks should traffic in <U where U : Q>, because that's
// what the base method declares.

// CHECK-LABEL: sil private [thunk] [ossa] @$s24vtable_generic_signature019ConcreteDerivedFromD4BaseC1fyyxAA1PRzlFAA0dG0CADyyxAA1QRzlFTV : $@convention(method) <U where U : Q> (@in_guaranteed U, @guaranteed ConcreteDerivedFromConcreteBase) -> ()
// CHECK-LABEL: sil private [thunk] [ossa] @$s24vtable_generic_signature30GenericDerivedFromConcreteBaseC1fyyqd__AA1PRd__lFAA0gH0CADyyxAA1QRzlFTV : $@convention(method) <T><U where U : Q> (@in_guaranteed U, @guaranteed GenericDerivedFromConcreteBase<T>) -> ()
// CHECK-LABEL: sil private [thunk] [ossa] @$s24vtable_generic_signature30ConcreteDerivedFromGenericBaseC1fyyxAA1PRzlFAA0gH0CADyyqd__AA1QRd__lFTV : $@convention(method) <U where U : Q> (@in_guaranteed U, @guaranteed ConcreteDerivedFromGenericBase) -> ()
// CHECK-LABEL: sil private [thunk] [ossa] @$s24vtable_generic_signature018GenericDerivedFromD4BaseC1fyyqd__AA1PRd__lFAA0dG0CADyyqd__AA1QRd__lFTV : $@convention(method) <T><U where U : Q> (@in_guaranteed U, @guaranteed GenericDerivedFromGenericBase<T>) -> ()

// CHECK-LABEL: sil_vtable ConcreteBase {
// CHECK-NEXT:   #ConcreteBase.f!1: <U where U : Q> (ConcreteBase) -> (U) -> () : @$s24vtable_generic_signature12ConcreteBaseC1fyyxAA1QRzlF
// CHECK-NEXT:   #ConcreteBase.init!allocator.1: (ConcreteBase.Type) -> () -> ConcreteBase : @$s24vtable_generic_signature12ConcreteBaseCACycfC
// CHECK-NEXT:   #ConcreteBase.deinit!deallocator.1: @$s24vtable_generic_signature12ConcreteBaseCfD
// CHECK-NEXT: }

// CHECK-LABEL: sil_vtable ConcreteDerivedFromConcreteBase {
// CHECK-NEXT:   #ConcreteBase.f!1: <U where U : Q> (ConcreteBase) -> (U) -> () : @$s24vtable_generic_signature019ConcreteDerivedFromD4BaseC1fyyxAA1PRzlFAA0dG0CADyyxAA1QRzlFTV [override]
// CHECK-NEXT:   #ConcreteBase.init!allocator.1: (ConcreteBase.Type) -> () -> ConcreteBase : @$s24vtable_generic_signature019ConcreteDerivedFromD4BaseCACycfC [override]
// CHECK-NEXT:   #ConcreteDerivedFromConcreteBase.f!1: <U where U : P> (ConcreteDerivedFromConcreteBase) -> (U) -> () : @$s24vtable_generic_signature019ConcreteDerivedFromD4BaseC1fyyxAA1PRzlF
// CHECK-NEXT:   #ConcreteDerivedFromConcreteBase.deinit!deallocator.1: @$s24vtable_generic_signature019ConcreteDerivedFromD4BaseCfD
// CHECK-NEXT: }

// CHECK-LABEL: sil_vtable GenericDerivedFromConcreteBase {
// CHECK-NEXT:   #ConcreteBase.f!1: <U where U : Q> (ConcreteBase) -> (U) -> () : @$s24vtable_generic_signature30GenericDerivedFromConcreteBaseC1fyyqd__AA1PRd__lFAA0gH0CADyyxAA1QRzlFTV [override]
// CHECK-NEXT:   #ConcreteBase.init!allocator.1: (ConcreteBase.Type) -> () -> ConcreteBase : @$s24vtable_generic_signature30GenericDerivedFromConcreteBaseCACyxGycfC [override]
// CHECK-NEXT:   #GenericDerivedFromConcreteBase.f!1: <T><U where U : P> (GenericDerivedFromConcreteBase<T>) -> (U) -> () : @$s24vtable_generic_signature30GenericDerivedFromConcreteBaseC1fyyqd__AA1PRd__lF
// CHECK-NEXT:   #GenericDerivedFromConcreteBase.deinit!deallocator.1: @$s24vtable_generic_signature30GenericDerivedFromConcreteBaseCfD
// CHECK-NEXT: }

// CHECK-LABEL: sil_vtable GenericBase {
// CHECK-NEXT:   #GenericBase.f!1: <T><U where U : Q> (GenericBase<T>) -> (U) -> () : @$s24vtable_generic_signature11GenericBaseC1fyyqd__AA1QRd__lF
// CHECK-NEXT:   #GenericBase.init!allocator.1: <T> (GenericBase<T>.Type) -> () -> GenericBase<T> : @$s24vtable_generic_signature11GenericBaseCACyxGycfC
// CHECK-NEXT:   #GenericBase.deinit!deallocator.1: @$s24vtable_generic_signature11GenericBaseCfD
// CHECK-NEXT: }

// CHECK-LABEL: sil_vtable ConcreteDerivedFromGenericBase {
// CHECK-NEXT:   #GenericBase.f!1: <T><U where U : Q> (GenericBase<T>) -> (U) -> () : @$s24vtable_generic_signature30ConcreteDerivedFromGenericBaseC1fyyxAA1PRzlFAA0gH0CADyyqd__AA1QRd__lFTV [override]
// CHECK-NEXT:   #GenericBase.init!allocator.1: <T> (GenericBase<T>.Type) -> () -> GenericBase<T> : @$s24vtable_generic_signature30ConcreteDerivedFromGenericBaseCACycfC [override]
// CHECK-NEXT:   #ConcreteDerivedFromGenericBase.f!1: <U where U : P> (ConcreteDerivedFromGenericBase) -> (U) -> () : @$s24vtable_generic_signature30ConcreteDerivedFromGenericBaseC1fyyxAA1PRzlF
// CHECK-NEXT:   #ConcreteDerivedFromGenericBase.deinit!deallocator.1: @$s24vtable_generic_signature30ConcreteDerivedFromGenericBaseCfD
// CHECK-NEXT: }

// CHECK-LABEL: sil_vtable GenericDerivedFromGenericBase {
// CHECK-NEXT:   #GenericBase.f!1: <T><U where U : Q> (GenericBase<T>) -> (U) -> () : @$s24vtable_generic_signature018GenericDerivedFromD4BaseC1fyyqd__AA1PRd__lFAA0dG0CADyyqd__AA1QRd__lFTV [override]
// CHECK-NEXT:   #GenericBase.init!allocator.1: <T> (GenericBase<T>.Type) -> () -> GenericBase<T> : @$s24vtable_generic_signature018GenericDerivedFromD4BaseCACyxGycfC [override]
// CHECK-NEXT:   #GenericDerivedFromGenericBase.f!1: <T><U where U : P> (GenericDerivedFromGenericBase<T>) -> (U) -> () : @$s24vtable_generic_signature018GenericDerivedFromD4BaseC1fyyqd__AA1PRd__lF
// CHECK-NEXT:   #GenericDerivedFromGenericBase.deinit!deallocator.1: @$s24vtable_generic_signature018GenericDerivedFromD4BaseCfD
// CHECK-NEXT: }
