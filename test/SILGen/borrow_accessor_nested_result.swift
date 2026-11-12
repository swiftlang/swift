// RUN: %target-swift-frontend -emit-sil %s -module-name borrow_accessor_nested_result -o /dev/null -Xllvm -sil-print-after=SILGenCleanup 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen %s -module-name borrow_accessor_nested_result | %FileCheck %s --check-prefix=CHECK-SILGEN

public class Klass {
  public init() {}
}

public struct Inner {
  var _i: Int
  var _k: Klass

  public var i: Int {
    borrow {
      return _i
    }
  }

  public var k: Klass {
    borrow {
      return _k
    }
  }
}

public struct Loadable {
  var _inner: Inner

  public var inner: Inner {
    borrow {
      return _inner
    }
  }

  public var nestedK: Klass {
    borrow {
      return inner.k
    }
  }

  public var nestedI: Int {
    borrow {
      return inner.i
    }
  }

  public var neverK: Klass {
    borrow {
      let i = inner.i
      fatalError("\(i)")
    }
  }
}

public struct AddressOnly<T> {
  var _inner: Inner
  var _t: T

  public var inner: Inner {
    borrow {
      return _inner
    }
  }

  public var nestedK: Klass {
    borrow {
      return inner.k
    }
  }

  public var nestedI: Int {
    borrow {
      return inner.i
    }
  }
}

public struct Projecting {
  var _inner: Inner

  public var inner: Inner {
    borrow {
      return _inner
    }
  }

  public var projectedK: Klass {
    borrow {
      return inner._k
    }
  }

  public var offPathK: Klass {
    borrow {
      let i = inner.i
      precondition(i >= 0)
      return _inner._k
    }
  }

  public var directK: Klass {
    borrow {
      return _inner._k
    }
  }
}

public struct PointerInner<Element: ~Copyable> {
  var _ptr: UnsafeMutablePointer<Element>
  var _k: Klass

  public var value: Element {
    unsafeAddress {
      return UnsafePointer(_ptr)
    }
  }
}

public struct PointerOuter<Element: ~Copyable>: ~Copyable {
  var _inner: PointerInner<Element>
  var _extra: Element?

  public var value: Element {
    @_unsafeSelfDependentResult
    borrow {
      return _inner.value
    }
  }
}

public struct MutablePointer {
  var _ptr: UnsafeMutablePointer<Klass>
  var _k: Klass

  public var value: Klass {
    @_unsafeSelfDependentResult
    borrow {
      return _ptr.pointee
    }
    @_unsafeSelfDependentResult
    nonmutating mutate {
      return &_ptr.pointee
    }
  }
}

public protocol NonmutatingMutate {
  var value: Klass { borrow nonmutating mutate }
}

extension MutablePointer: NonmutatingMutate {}

public struct AddressOnlyMutablePointer<T> {
  var _pointer: MutablePointer
  var _t: T

  public var value: Klass {
    borrow {
      return _pointer.value
    }
    nonmutating mutate {
      return &_pointer.value
    }
  }
}

public final class Storage<T> {
  let t: T

  init(_ t: T) { self.t = t }
}

public struct ClassField<T> {
  var _s: Storage<T>
  var _t: T

  public var value: T {
    borrow {
      return _s.t
    }
  }
}

public struct AddressOnlyPointerInner<Element: ~Copyable> {
  var _ptr: UnsafeMutablePointer<Element>
  var _any: Any

  public var value: Element {
    unsafeAddress {
      return UnsafePointer(_ptr)
    }
  }
}

public final class PointerStorage<Element: ~Copyable> {
  let inner: AddressOnlyPointerInner<Element>

  init(_ inner: AddressOnlyPointerInner<Element>) { self.inner = inner }
}

public struct ClassPointerOuter<Element: ~Copyable>: ~Copyable {
  var _s: PointerStorage<Element>
  var _extra: Element?

  public var value: Element {
    @_unsafeSelfDependentResult
    borrow {
      return _s.inner.value
    }
  }
}

// CHECK-LABEL: sil [ossa] @$s29borrow_accessor_nested_result8LoadableV0C1KAA5KlassCvb : $@convention(method) (@guaranteed Loadable) -> @guaranteed Klass {
// CHECK:   [[INNER:%.*]] = apply {{%.*}}(%0) : $@convention(method) (@guaranteed Loadable) -> @guaranteed Inner
// CHECK-NOT: unchecked_ownership
// CHECK:   [[K:%.*]] = apply {{%.*}}([[INNER]]) : $@convention(method) (@guaranteed Inner) -> @guaranteed Klass
// CHECK-NEXT: return_borrow [[K]] from_scopes ([[K]], [[INNER]])
// CHECK: } // end sil function '$s29borrow_accessor_nested_result8LoadableV0C1KAA5KlassCvb'

// CHECK-LABEL: sil [ossa] @$s29borrow_accessor_nested_result8LoadableV0C1ISivb : $@convention(method) (@guaranteed Loadable) -> @guaranteed Int {
// CHECK:   [[INNER:%.*]] = apply {{%.*}}(%0) : $@convention(method) (@guaranteed Loadable) -> @guaranteed Inner
// CHECK-NOT: unchecked_ownership
// CHECK:   [[I:%.*]] = apply {{%.*}}([[INNER]]) : $@convention(method) (@guaranteed Inner) -> @guaranteed Int
// CHECK-NEXT: end_borrow [[INNER]]
// CHECK-NEXT: return [[I]]
// CHECK: } // end sil function '$s29borrow_accessor_nested_result8LoadableV0C1ISivb'

// A borrow accessor that never returns has no result to enclose, but the
// unchecked_ownership of a nested result is still removed.
// CHECK-LABEL: sil [ossa] @$s29borrow_accessor_nested_result8LoadableV6neverKAA5KlassCvb : $@convention(method) (@guaranteed Loadable) -> @guaranteed Klass {
// CHECK:   [[INNER:%.*]] = apply {{%.*}}(%0) : $@convention(method) (@guaranteed Loadable) -> @guaranteed Inner
// CHECK-NOT: unchecked_ownership
// CHECK:   unreachable
// CHECK: } // end sil function '$s29borrow_accessor_nested_result8LoadableV6neverKAA5KlassCvb'

// CHECK-LABEL: sil [ossa] @$s29borrow_accessor_nested_result11AddressOnlyV0C1KAA5KlassCvb : $@convention(method) <T> (@in_guaranteed AddressOnly<T>) -> @guaranteed Klass {
// CHECK:   [[INNER:%.*]] = apply {{%.*}}<T>(%0) : $@convention(method) <τ_0_0> (@in_guaranteed AddressOnly<τ_0_0>) -> @guaranteed Inner
// CHECK-NOT: unchecked_ownership
// CHECK:   [[K:%.*]] = apply {{%.*}}([[INNER]]) : $@convention(method) (@guaranteed Inner) -> @guaranteed Klass
// CHECK-NEXT: return_borrow [[K]] from_scopes ([[K]], [[INNER]])
// CHECK: } // end sil function '$s29borrow_accessor_nested_result11AddressOnlyV0C1KAA5KlassCvb'

// CHECK-LABEL: sil [ossa] @$s29borrow_accessor_nested_result11AddressOnlyV0C1ISivb : $@convention(method) <T> (@in_guaranteed AddressOnly<T>) -> @guaranteed Int {
// CHECK:   [[INNER:%.*]] = apply {{%.*}}<T>(%0) : $@convention(method) <τ_0_0> (@in_guaranteed AddressOnly<τ_0_0>) -> @guaranteed Inner
// CHECK-NOT: unchecked_ownership
// CHECK:   [[I:%.*]] = apply {{%.*}}([[INNER]]) : $@convention(method) (@guaranteed Inner) -> @guaranteed Int
// CHECK-NEXT: end_borrow [[INNER]]
// CHECK-NEXT: return [[I]]
// CHECK: } // end sil function '$s29borrow_accessor_nested_result11AddressOnlyV0C1ISivb'

// The fixup looks through a projection of the result of a nested borrow
// accessor to its scope.
// CHECK-LABEL: sil [ossa] @$s29borrow_accessor_nested_result10ProjectingV10projectedKAA5KlassCvb : $@convention(method) (@guaranteed Projecting) -> @guaranteed Klass {
// CHECK:   [[INNER:%.*]] = apply {{%.*}}(%0) : $@convention(method) (@guaranteed Projecting) -> @guaranteed Inner
// CHECK-NEXT: [[K:%.*]] = struct_extract [[INNER]], #Inner._k
// CHECK-NEXT: return_borrow [[K]] from_scopes ([[INNER]])
// CHECK: } // end sil function '$s29borrow_accessor_nested_result10ProjectingV10projectedKAA5KlassCvb'

// A scope that does not enclose the returned value keeps its end_borrow, and
// its unchecked_ownership is removed.
// CHECK-LABEL: sil [ossa] @$s29borrow_accessor_nested_result10ProjectingV8offPathKAA5KlassCvb : $@convention(method) (@guaranteed Projecting) -> @guaranteed Klass {
// CHECK:   [[INNER:%.*]] = apply {{%.*}}(%0) : $@convention(method) (@guaranteed Projecting) -> @guaranteed Inner
// CHECK-NEXT: copy_value [[INNER]]
// CHECK-NEXT: end_borrow [[INNER]]
// CHECK:   [[I:%.*]] = struct_extract %0, #Projecting._inner
// CHECK-NEXT: [[K:%.*]] = struct_extract [[I]], #Inner._k
// CHECK:   return [[K]]
// CHECK: } // end sil function '$s29borrow_accessor_nested_result10ProjectingV8offPathKAA5KlassCvb'

// A result that is only enclosed by the @guaranteed self is returned with a
// plain return.
// CHECK-LABEL: sil [ossa] @$s29borrow_accessor_nested_result10ProjectingV7directKAA5KlassCvb : $@convention(method) (@guaranteed Projecting) -> @guaranteed Klass {
// CHECK:   [[I:%.*]] = struct_extract %0, #Projecting._inner
// CHECK-NEXT: [[K:%.*]] = struct_extract [[I]], #Inner._k
// CHECK-NEXT: return [[K]]
// CHECK: } // end sil function '$s29borrow_accessor_nested_result10ProjectingV7directKAA5KlassCvb'

// The address result of an unsafe addressor depends on the addressor's self,
// which is borrowed within the borrow accessor.
// CHECK-LABEL: sil [ossa] @$s29borrow_accessor_nested_result12PointerOuterVAARi_zrlE5valuexvb : $@convention(method) <Element where Element : ~Copyable> (@in_guaranteed PointerOuter<Element>) -> @guaranteed_address Element {
// CHECK:   [[LB:%.*]] = load_borrow
// CHECK:   [[MD:%.*]] = mark_dependence [unresolved] {{%.*}} on [[LB]]
// CHECK-NEXT: [[A:%.*]] = begin_access [read] [unsafe] [[MD]]
// CHECK-NEXT: [[R:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[A]]
// CHECK-NEXT: end_access [[A]]
// CHECK-NEXT: return_borrow [[R]] from_scopes ([[LB]])
// CHECK: } // end sil function '$s29borrow_accessor_nested_result12PointerOuterVAARi_zrlE5valuexvb'

// The @inout result of a mutate accessor with an object self is returned from
// the borrow scope of self, in a witness thunk and in a mutate accessor.
// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] [ossa] @$s29borrow_accessor_nested_result14MutablePointerVAA17NonmutatingMutateA2aDP5valueAA5KlassCvzTW : $@convention(witness_method: NonmutatingMutate) (@in_guaranteed MutablePointer) -> @inout Klass {
// CHECK:   [[LB:%.*]] = load_borrow %0
// CHECK:   [[R:%.*]] = apply {{%.*}}([[LB]]) : $@convention(method) (@guaranteed MutablePointer) -> @inout Klass
// CHECK-NEXT: return_borrow [[R]] from_scopes ([[LB]])
// CHECK: } // end sil function '$s29borrow_accessor_nested_result14MutablePointerVAA17NonmutatingMutateA2aDP5valueAA5KlassCvzTW'

// CHECK-LABEL: sil [ossa] @$s29borrow_accessor_nested_result25AddressOnlyMutablePointerV5valueAA5KlassCvz : $@convention(method) <T> (@in_guaranteed AddressOnlyMutablePointer<T>) -> @inout Klass {
// CHECK:   [[LB:%.*]] = load_borrow
// CHECK:   [[R:%.*]] = apply {{%.*}}([[LB]]) : $@convention(method) (@guaranteed MutablePointer) -> @inout Klass
// CHECK-NOT: end_borrow
// CHECK:   return_borrow [[R]] from_scopes ([[LB]])
// CHECK: } // end sil function '$s29borrow_accessor_nested_result25AddressOnlyMutablePointerV5valueAA5KlassCvz'

// The address result of a class field depends on the class reference, which is
// borrowed within the borrow accessor.
// CHECK-LABEL: sil [ossa] @$s29borrow_accessor_nested_result10ClassFieldV5valuexvb : $@convention(method) <T> (@in_guaranteed ClassField<T>) -> @guaranteed_address T {
// CHECK:   [[LB:%.*]] = load_borrow
// CHECK-NEXT: [[R:%.*]] = ref_element_addr [[LB]], #Storage.t
// CHECK-NEXT: return_borrow [[R]] from_scopes ([[LB]])
// CHECK: } // end sil function '$s29borrow_accessor_nested_result10ClassFieldV5valuexvb'

// The address result of an unsafe addressor depends on the addressor's address
// self, a class field whose class reference is borrowed within the borrow
// accessor.
// CHECK-LABEL: sil [ossa] @$s29borrow_accessor_nested_result17ClassPointerOuterVAARi_zrlE5valuexvb : $@convention(method) <Element where Element : ~Copyable> (@in_guaranteed ClassPointerOuter<Element>) -> @guaranteed_address Element {
// CHECK:   [[LB:%.*]] = load_borrow
// CHECK-NEXT: [[SELF:%.*]] = ref_element_addr [[LB]], #PointerStorage.inner
// CHECK:   [[MD:%.*]] = mark_dependence [unresolved] {{%.*}} on [[SELF]]
// CHECK-NEXT: [[A:%.*]] = begin_access [read] [unsafe] [[MD]]
// CHECK-NEXT: [[R:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[A]]
// CHECK-NEXT: end_access [[A]]
// CHECK-NEXT: return_borrow [[R]] from_scopes ([[LB]])
// CHECK: } // end sil function '$s29borrow_accessor_nested_result17ClassPointerOuterVAARi_zrlE5valuexvb'

// SILGen marks the result of a borrow accessor called in a borrow accessor
// with unchecked_ownership, unless the result is trivial.
// CHECK-SILGEN-LABEL: sil [ossa] @$s29borrow_accessor_nested_result8LoadableV0C1KAA5KlassCvb : $@convention(method) (@guaranteed Loadable) -> @guaranteed Klass {
// CHECK-SILGEN:   [[INNER:%.*]] = apply {{%.*}}(%0) : $@convention(method) (@guaranteed Loadable) -> @guaranteed Inner
// CHECK-SILGEN-NEXT: [[U:%.*]] = unchecked_ownership [[INNER]]
// CHECK-SILGEN:   [[K:%.*]] = apply {{%.*}}([[U]]) : $@convention(method) (@guaranteed Inner) -> @guaranteed Klass
// CHECK-SILGEN-NEXT: [[UK:%.*]] = unchecked_ownership [[K]]
// CHECK-SILGEN-NEXT: end_borrow [[K]]
// CHECK-SILGEN-NEXT: end_borrow [[INNER]]
// CHECK-SILGEN:   return [[UK]]
// CHECK-SILGEN: } // end sil function '$s29borrow_accessor_nested_result8LoadableV0C1KAA5KlassCvb'

// CHECK-SILGEN-LABEL: sil [ossa] @$s29borrow_accessor_nested_result8LoadableV0C1ISivb : $@convention(method) (@guaranteed Loadable) -> @guaranteed Int {
// CHECK-SILGEN:   [[INNER:%.*]] = apply {{%.*}}(%0) : $@convention(method) (@guaranteed Loadable) -> @guaranteed Inner
// CHECK-SILGEN-NEXT: [[U:%.*]] = unchecked_ownership [[INNER]]
// CHECK-SILGEN:   [[I:%.*]] = apply {{%.*}}([[U]]) : $@convention(method) (@guaranteed Inner) -> @guaranteed Int
// CHECK-SILGEN-NEXT: end_borrow [[INNER]]
// CHECK-SILGEN:   return [[I]]
// CHECK-SILGEN: } // end sil function '$s29borrow_accessor_nested_result8LoadableV0C1ISivb'
