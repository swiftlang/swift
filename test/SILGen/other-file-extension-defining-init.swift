// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Stored properties with default initialization expressions can have those expressions be "subsumed" by other
// computed properties that define an init accessor that itself has a default value.
//
// This test specifically exercises the case where the type is defined in a secondary file, while the initializer
// is defined in a separate file designated as primary during compilation. This normally shouldn't matter, but due
// to some quirks with accessors and how Sema treats types in secondary files, we can see incorrect answers in SILGen.
// https://github.com/swiftlang/swift/issues/91700

// RUN: %target-swift-emit-silgen -primary-file %t/Extension.swift %t/Test.swift -module-name initaccessors > %t/output.silgen
// RUN: %FileCheck %s < %t/output.silgen
// RUN: %target-swift-emit-sil -sil-verify-all -primary-file %t/Extension.swift %t/Test.swift -module-name initaccessors > /dev/null

//--- Test.swift

// The names of the structs below refer to whether 'facade' has a default initial value or not.

// Control: just an ordinary stored property.
public struct Control {
  var storage = 7
}

public struct NoDefault {
  var storage = 7

  var facade: Int { // <--
    @storageRestrictions(initializes: storage)
    init(initialValue) { storage = initialValue }
    get { storage }
    set { storage = newValue }
  }
}

public struct ExplicitDefault {
  var storage = 1

  var facade: Int = 2 { // <--
    @storageRestrictions(initializes: storage)
    init(initialValue) { storage = initialValue }
    get { storage }
    set { storage = newValue }
  }
}

// facade's default initialization expression is synthesized (an implicit 'nil' assignment),
// and it still subsumes 'storage', so a cross-file initializer must route through the accessor.
public struct ImplicitDefault {
  var storage: Int? = 7

  var facade: Int? {  // <--
    @storageRestrictions(initializes: storage)
    init(initialValue) { storage = initialValue }
    get { storage }
  }
}

// Two init accessors initializing the same storage.
// One that does have a default and thus subsumes, and the other without.
public struct TwoAccessorsOneDefault {
  var storage = 0

  var facade_noDefault: Int { // <--
    @storageRestrictions(initializes: storage)
    init(initialValue) { storage = initialValue }
    get { storage }
    set { storage = newValue }
  }

  var facade_withDefault: Int = 42 { // <--
    @storageRestrictions(initializes: storage)
    init(initialValue) { storage = initialValue }
    get { storage }
    set { storage = newValue }
  }
}

public struct TwoAccessorsTwoDefaults {
  var storage = 0

  var facade1: Int = 520 { // <--
    @storageRestrictions(initializes: storage)
    init(initialValue) { storage = initialValue }
    get { storage }
    set { storage = newValue }
  }

  var facade2: Int = 42 { // <--
    @storageRestrictions(initializes: storage)
    init(initialValue) { storage = initialValue }
    get { storage }
    set { storage = newValue }
  }
}

// A generic type: same secondary-file subsumption, exercising the generic path.
public struct GenericExplicitDefault<T> {
  var value: T

  var storage = 1

  var facade: Int = 2 { // <--
    @storageRestrictions(initializes: storage)
    init(initialValue) { storage = initialValue }
    get { storage }
    set { storage = newValue }
  }
}

// An init accessor that *accesses* a stored property subsumed by another
// accessor must not raw-store that storage via the "emit accessed properties
// first" ordering path (a second place the subsumed initializer could leak).
public struct AccessesSubsumed {
  var storage = 1

  var facade: Int = 2 { // <-- subsumes storage
    @storageRestrictions(initializes: storage)
    init(initialValue) { storage = initialValue }
    get { storage }
    set { storage = newValue }
  }

  var reader: Int = 0 { // <-- accesses (reads) storage; declared after facade
    @storageRestrictions(accesses: storage)
    init(initialValue) {}
    get { storage }
    set {}
  }
}

//--- Extension.swift

// Without any init accessors, we trigger the initialization of the storage across files.
extension Control { init(other: Int) {} }
// CHECK-LABEL: sil hidden [ossa] @$s13initaccessors7ControlV5otherACSi_tcfC :
// CHECK-NOT:     assign_or_init
// CHECK:         struct_element_addr {{.*}}, #Control.storage
// CHECK:         function_ref @$s13initaccessors7ControlV7storageSivpfi
// CHECK-NOT:     assign_or_init
// CHECK:       } // end sil function '$s13initaccessors7ControlV5otherACSi_tcfC'


// facade has no default of its own, so storage's initialization is not 'subsumed',
// meaning we trigger the storage's initialization expression.
extension NoDefault { init(other: Int) {} }
// CHECK-LABEL: sil hidden [ossa] @$s13initaccessors9NoDefaultV5otherACSi_tcfC :
// CHECK-NOT:     assign_or_init
// CHECK:         struct_element_addr {{.*}}, #NoDefault.storage
// CHECK:         function_ref @$s13initaccessors9NoDefaultV7storageSivpfi
// CHECK-NOT:     assign_or_init
// CHECK:       } // end sil function '$s13initaccessors9NoDefaultV5otherACSi_tcfC'


// For all the rest, the storage' initialization expression is subsumed by the accessor,
// so expect to see no mention of the storage, and only assign_or_init of the facades.

extension ExplicitDefault { init(other: Int) {} }
// CHECK-LABEL: sil hidden [ossa] @$s13initaccessors15ExplicitDefaultV5otherACSi_tcfC  :
// CHECK-NOT:     storage
// CHECK:         assign_or_init #ExplicitDefault.facade
// CHECK-NOT:     storage
// CHECK:       } // end sil function '$s13initaccessors15ExplicitDefaultV5otherACSi_tcfC'

extension ImplicitDefault { init(other: Int) {} }
// CHECK-LABEL: sil hidden [ossa] @$s13initaccessors15ImplicitDefaultV5otherACSi_tcfC :
// CHECK-NOT:     storage
// CHECK:         assign_or_init #ImplicitDefault.facade
// CHECK-NOT:     storage
// CHECK:       } // end sil function '$s13initaccessors15ImplicitDefaultV5otherACSi_tcfC'

// Two accessors, but only one had a default. Storage's init is subsumed and we trigger only one accessor's initialization.
extension TwoAccessorsOneDefault { init(other: Int) {} }
// CHECK-LABEL: sil hidden [ossa] @$s13initaccessors22TwoAccessorsOneDefaultV5otherACSi_tcfC :
// CHECK-NOT:     assign_or_init
// CHECK:         assign_or_init #TwoAccessorsOneDefault.facade_withDefault
// CHECK-NOT:     assign_or_init
// CHECK:       } // end sil function '$s13initaccessors22TwoAccessorsOneDefaultV5otherACSi_tcfC'

// Two accessors had defaults, so we get two assign_or_inits in sequence.
extension TwoAccessorsTwoDefaults { init(other: Int) {} }
// CHECK-LABEL: sil hidden [ossa] @$s13initaccessors012TwoAccessorsB8DefaultsV5otherACSi_tcfC :
// CHECK-NOT:     storage
// CHECK:         [[ONE_INIT:%[0-9]+]] = function_ref @$s13initaccessors012TwoAccessorsB8DefaultsV7facade1Sivi
// CHECK:         [[ONE_INIT_PA:%[0-9]+]] = partial_apply [callee_guaranteed] [on_stack] [[ONE_INIT]](
// CHECK:         assign_or_init #TwoAccessorsTwoDefaults.facade1, self {{.*}}, value {{.*}}, init [[ONE_INIT_PA]]
// CHECK-NOT:     assign_or_init
// CHECK:         [[TWO_INIT:%[0-9]+]] = function_ref @$s13initaccessors012TwoAccessorsB8DefaultsV7facade2Sivi
// CHECK:         [[TWO_INIT_PA:%[0-9]+]] = partial_apply [callee_guaranteed] [on_stack] [[TWO_INIT]](
// CHECK:         assign_or_init #TwoAccessorsTwoDefaults.facade2, self {{.*}}, value {{.*}}, init [[TWO_INIT_PA]]
// CHECK-NOT:     assign_or_init
// CHECK:       } // end sil function '$s13initaccessors012TwoAccessorsB8DefaultsV5otherACSi_tcfC'

// A generic type: facade's default subsumes storage, so the cross-file init
// routes through the accessor with no mention of storage.
extension GenericExplicitDefault { init(other: T) { value = other } }
// CHECK-LABEL: sil hidden [ossa] @$s13initaccessors22GenericExplicitDefaultV5otherACyxGx_tcfC :
// CHECK-NOT:     storage
// CHECK:         assign_or_init #GenericExplicitDefault.facade
// CHECK-NOT:     storage
// CHECK:       } // end sil function '$s13initaccessors22GenericExplicitDefaultV5otherACyxGx_tcfC'

// The 'reader' accessor accesses 'storage', but 'storage' is subsumed by
// 'facade', so the cross-file init must route it through 'facade' -- not
// raw-store it while ordering 'reader's accessed properties.
extension AccessesSubsumed { init(other: Int) {} }
// CHECK-LABEL: sil hidden [ossa] @$s13initaccessors16AccessesSubsumedV5otherACSi_tcfC :
// CHECK-NOT:     storage
// CHECK:         assign_or_init #AccessesSubsumed.facade
// CHECK-NOT:     storage
// CHECK:         assign_or_init #AccessesSubsumed.reader
// CHECK-NOT:     storage
// CHECK:       } // end sil function '$s13initaccessors16AccessesSubsumedV5otherACSi_tcfC'
