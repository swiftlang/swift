// RUN: %target-typecheck-verify-swift \
// RUN:   -enable-experimental-feature LifetimeDependence  \
// RUN:   -debug-diagnostic-names -target arm64-apple-macos14.4 

// REQUIRES: swift_feature_LifetimeDependence

// REQUIRES: OS=macosx || OS=ios || OS=tvos || OS=watchOS || OS=xros

protocol P {}
struct NCG<T: ~Copyable> {}
extension NCG: P where T: Copyable {} // expected-note 2{{requirement_implied_by_conditional_conformance}}

struct NEG<T: ~Escapable> {}
extension NEG: P {} // expected-note {{requirement_implied_by_conditional_conformance}}

struct All {}
struct NoCopy: ~Copyable {}
struct NoEscape: ~Escapable {}



/// MARK: dynamic casts are gated by availability. Older runtimes don't check
/// for conformance to Copyable so they'll give bogus results.

// expected-note@+1 8{{availability_add_attribute}}
func dyn_cast_errors<T: ~Copyable, V: ~Escapable>(
                    _ generic: NCG<T>, _ concrete: NCG<NoCopy>,
                    _ genericEsc: NEG<V>, _ concreteEsc: NEG<NoEscape>) {
  _ = concrete as? any P // expected-error {{availability_copyable_generics_casting_only_version_newer}} // expected-note {{availability_guard_with_version_check}}
  _ = generic as? any P // expected-error {{availability_copyable_generics_casting_only_version_newer}} // expected-note {{availability_guard_with_version_check}}

  _ = concrete is any P // expected-error {{availability_copyable_generics_casting_only_version_newer}} // expected-note {{availability_guard_with_version_check}}
  _ = generic is any P // expected-error {{availability_copyable_generics_casting_only_version_newer}} // expected-note {{availability_guard_with_version_check}}

  _ = concrete as! any P // expected-error {{availability_copyable_generics_casting_only_version_newer}} // expected-note {{availability_guard_with_version_check}}
  _ = generic as! any P // expected-error {{availability_copyable_generics_casting_only_version_newer}} // expected-note {{availability_guard_with_version_check}}

  _ = genericEsc as? any P // expected-error {{availability_escapable_generics_casting_only_version_newer}} // expected-note {{availability_guard_with_version_check}}
  _ = concreteEsc is any P // expected-error {{availability_escapable_generics_casting_only_version_newer}} // expected-note {{availability_guard_with_version_check}}
}

@available(SwiftStdlib 6.0, *)
func FIXED_dyn_cast_errors<T: ~Copyable, V: ~Escapable>(
                          _ generic: NCG<T>, _ concrete: NCG<NoCopy>,
                          _ genericEsc: NEG<V>, _ concreteEsc: NEG<NoEscape>) {
  _ = concrete as? any P
  _ = generic as? any P

  _ = concrete is any P
  _ = generic is any P

  _ = concrete as! any P
  _ = generic as! any P

  _ = genericEsc as? any P
  _ = concreteEsc is any P
}

func noAvailabilityNeeded<T>(_ generic: NCG<T>, _ concrete: NCG<All>) {
  _ = concrete as? any P // expected-warning {{conditional_downcast_coercion}}
  _ = generic as? any P  // expected-warning {{conditional_downcast_coercion}}

  _ = concrete is any P  // expected-warning {{isa_is_always_true}}
  _ = generic is any P   // expected-warning {{isa_is_always_true}}

  _ = concrete as! any P // expected-warning {{forced_downcast_coercion}}
  _ = generic as! any P  // expected-warning {{forced_downcast_coercion}}

  _ = concrete as any P
  _ = generic as any P

  _ = concrete as Any
  _ = generic as Any
}

func expected_checked_cast_errors<T: ~Copyable>(_ generic: NCG<T>, _ concrete: NCG<NoCopy>,
                                                _ concreteEsc: NEG<NoEscape>) {
  _ = concrete as any P // expected-error {{type_does_not_conform_decl_owner}}
  _ = generic as any P  // expected-error {{type_does_not_conform_decl_owner}}
  _ = concreteEsc as any P // expected-error {{type_does_not_conform_decl_owner}}
}

/// MARK: existential erasure requires availability, because later dynamic casts
/// of that erased type will not correctly check for Copyable generic args.

func eraseImplicit(_ a: Any) {}

// expected-note@+1 9{{availability_add_attribute}}
func erasure_cast_disallowed<T: ~Copyable>(_ generic: NCG<T>, _ concrete: NCG<NoCopy>, _ concreteEsc: NEG<NoEscape>) -> Any {
  _ = concrete as Any // expected-error {{availability_copyable_generics_casting_only_version_newer}} // expected-note {{availability_guard_with_version_check}}
  _ = concreteEsc as Any // expected-error {{availability_escapable_generics_casting_only_version_newer}} // expected-note {{availability_guard_with_version_check}}
  _ = generic as Any // expected-error {{availability_copyable_generics_casting_only_version_newer}} // expected-note {{availability_guard_with_version_check}}

  let _: Any = concrete // expected-error {{availability_copyable_generics_casting_only_version_newer}} // expected-note {{availability_guard_with_version_check}}
  let _: Any = generic // expected-error {{availability_copyable_generics_casting_only_version_newer}} // expected-note {{availability_guard_with_version_check}}

  let _: Any = { concreteEsc }() // expected-error {{availability_escapable_generics_casting_only_version_newer}} // expected-note {{availability_guard_with_version_check}}

  eraseImplicit(concrete) // expected-error {{availability_copyable_generics_casting_only_version_newer}} // expected-note {{availability_guard_with_version_check}}
  eraseImplicit(generic) // expected-error {{availability_copyable_generics_casting_only_version_newer}} // expected-note {{availability_guard_with_version_check}}

  return concrete // expected-error {{availability_copyable_generics_casting_only_version_newer}} // expected-note {{availability_guard_with_version_check}}
}

struct Box<Wrapped: ~Copyable>: ~Copyable { // expected-note {{availability_add_attribute}}
  private let _pointer: UnsafeMutablePointer<Wrapped>

  init(_ element: consuming Wrapped) { // expected-note {{availability_add_attribute}}
    _pointer = .allocate(capacity: 1)
    print("allocating",_pointer) // expected-error {{availability_copyable_generics_casting_only_version_newer}} // expected-note {{availability_guard_with_version_check}}
    _pointer.initialize(to: element)
  }
}

/// MARK: misc. operations that are permitted

public protocol Debatable: ~Copyable {}

public func asExistential(_ t: consuming any Debatable & ~Copyable) {}

public func hello<T: Debatable & ~Copyable>(_ t: consuming T) {
  asExistential(t)
}

extension UnsafeMutableRawPointer {
  public func blahInitializeMemory<T: ~Copyable>(
      as type: T.Type, from source: UnsafeMutablePointer<T>, count: Int
    ) {
      _ = UnsafeMutableRawPointer(source + count)
  }
}
