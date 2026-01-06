// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend                                      \
// RUN:     %s                                                      \
// RUN:     -emit-sil -verify -verify-additional-prefix fragile-    \
// RUN:     -sil-verify-all                                         \
// RUN:     -module-name Library
// RUN: %target-swift-frontend                                      \
// RUN:     %s                                                      \
// RUN:     -emit-sil -verify -verify-additional-prefix resilient-  \
// RUN:     -sil-verify-all                                         \
// RUN:     -enable-library-evolution                               \
// RUN:     -module-name Library

public func take(_ u: consuming Ur) {}

public struct Ur : ~Copyable {
  @usableFromInline init() {}
  deinit {}
}

@usableFromInline
internal struct Agg_Internal_UFI : ~Copyable {
  @usableFromInline
  init(u1: consuming Ur, u2: consuming Ur) {
    self.u1 = u1
    self.u2 = u2
  }

  @usableFromInline
  internal var u1: Ur
  @usableFromInline
  internal var u2: Ur
}

@_alwaysEmitIntoClient
func piecewise_Agg_Internal_UFI(_ a: consuming Agg_Internal_UFI) {
  take(a.u1) // expected-fragile-error{{cannot partially consume 'a' of non-frozen usableFromInline type 'Agg_Internal_UFI' within a function annotated '@_alwaysEmitIntoClient'}}
             // expected-resilient-error@-1{{field 'a.u1' was consumed but not reinitialized; the field must be reinitialized during the access}}
             // expected-resilient-note@-2{{consumed here}}
  take(a.u2) // expected-fragile-error{{cannot partially consume 'a' of non-frozen usableFromInline type 'Agg_Internal_UFI' within a function annotated '@_alwaysEmitIntoClient'}}
             // expected-resilient-error@-1{{field 'a.u2' was consumed but not reinitialized; the field must be reinitialized during the access}}
             // expected-resilient-note@-2{{consumed here}}
}

@_alwaysEmitIntoClient
func get_Agg_Internal_UFI() -> Agg_Internal_UFI {
  return Agg_Internal_UFI(u1: Ur(), u2: Ur())
}

@_alwaysEmitIntoClient
public func call_piecewise_Agg_Internal_UFI() {
  let a = get_Agg_Internal_UFI()
  piecewise_Agg_Internal_UFI(a)
}

