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

// CHECK-LABEL: sil [ossa] @$s29borrow_accessor_nested_result8LoadableV0C1KAA5KlassCvb : $@convention(method) (@guaranteed Loadable) -> @guaranteed Klass {
// CHECK:   [[INNER:%.*]] = apply {{%.*}}(%0) : $@convention(method) (@guaranteed Loadable) -> @guaranteed Inner
// CHECK-NOT: unchecked_ownership
// CHECK:   [[K:%.*]] = apply {{%.*}}([[INNER]]) : $@convention(method) (@guaranteed Inner) -> @guaranteed Klass
// CHECK-NEXT: return_borrow [[K]] from_scopes ([[INNER]], [[K]])
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
// CHECK-NEXT: return_borrow [[K]] from_scopes ([[INNER]], [[K]])
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
