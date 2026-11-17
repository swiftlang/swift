// RUN: %target-typecheck-verify-swift -enable-experimental-feature Lifetimes

// REQUIRES: swift_feature_Lifetimes

// rdar://192958686: An override has the opaque read accessors of the property
// it overrides. A '_read' with a scoped dependence cannot provide a getter.

struct NE: ~Escapable {
  @_lifetime(immortal) init() {}
}

class OpaqueReadBase {
  var immortalRead: NE { @_lifetime(immortal) _read { yield NE() } } // expected-note {{attempt to override property here}}
  var immortalGet: NE { @_lifetime(immortal) get { NE() } } // expected-note {{attempt to override property here}}
  var scopedRead: NE { @_lifetime(borrow self) _read { yield NE() } }
}

class OpaqueReadDerived: OpaqueReadBase {
  override var immortalRead: NE { @_lifetime(borrow self) _read { yield NE() } } // expected-error {{property 'immortalRead' cannot override a property that is read through a getter, because its read accessor has a scoped lifetime dependence}}
  override var immortalGet: NE { @_lifetime(borrow self) _read { yield NE() } } // expected-error {{property 'immortalGet' cannot override a property that is read through a getter, because its read accessor has a scoped lifetime dependence}}
  // The base is read through a '_read', which is synthesized from the getter.
  override var scopedRead: NE { @_lifetime(borrow self) get { NE() } }
}
