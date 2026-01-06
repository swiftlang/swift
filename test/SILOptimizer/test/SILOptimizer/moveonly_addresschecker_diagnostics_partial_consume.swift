// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend                                      \
// RUN:     %t/Library.swift                                        \
// RUN:     -emit-module                                            \
// RUN:     -package-name Package                                   \
// RUN:     -module-name Library                                    \
// RUN:     -emit-module-path %t/Library.swiftmodule

// RUN: %target-swift-frontend                                      \
// RUN:     %t/Downstream.swift                                     \
// RUN:     -emit-sil -verify  -verify-additional-prefix fragile-   \
// RUN:     -sil-verify-all                                         \
// RUN:     -package-name Package                                   \
// RUN:     -debug-diagnostic-names                                 \
// RUN:     -I %t

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend                                      \
// RUN:     %t/Library.swift                                        \
// RUN:     -emit-module                                            \
// RUN:     -package-name Package                                   \
// RUN:     -enable-library-evolution                               \
// RUN:     -module-name Library                                    \
// RUN:     -emit-module-path %t/Library.swiftmodule

// RUN: %target-swift-frontend                                      \
// RUN:     %t/Downstream.swift                                     \
// RUN:     -emit-sil -verify  -verify-additional-prefix resilient- \
// RUN:     -sil-verify-all                                         \
// RUN:     -package-name Package                                   \
// RUN:     -debug-diagnostic-names                                 \
// RUN:     -I %t

//--- Library.swift

public func take(_ u: consuming Ur) {}

public struct Ur : ~Copyable {
  @usableFromInline init() {}
  deinit {}
}

// =====BEGIN======================= Agg_Public =============================={{

public struct Agg_Public : ~Copyable {
  public var u1: Ur
  public var u2: Ur
}

// =====END========================= Agg_Public ==============================}}

// =====BEGIN==================== Agg_Public_Frozen =========================={{

@frozen public struct Agg_Public_Frozen : ~Copyable {
  public var u1: Ur
  public var u2: Ur
}

// =====END====================== Agg_Public_Frozen ==========================}}

// =====BEGIN====================== Agg_Package =============================={{

package struct Agg_Package : ~Copyable {
  package var u1: Ur
  package var u2: Ur
}

// =====END======================== Agg_Package ==============================}}

// =====BEGIN=================== Agg_Package_Frozen =========================={{
// TODO: Uncomment when package decls can be marked @frozen.
//
// @frozen package struct Agg_Package_Frozen : ~Copyable {
//   package var u1: Ur
//   package var u2: Ur
// }
// 
// =====END===================== Agg_Package_Frozen ==========================}}

// =====BEGIN================ Agg_Internal_UFI_Frozen ========================{{

@usableFromInline
@frozen internal struct Agg_Internal_UFI_Frozen : ~Copyable {
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
func piecewise_Agg_Internal_UFI_Frozen(_ a: consuming Agg_Internal_UFI_Frozen) {
  take(a.u1)
  take(a.u2)
}

@_alwaysEmitIntoClient
func get_Agg_Internal_UFI_Frozen() -> Agg_Internal_UFI_Frozen {
  return Agg_Internal_UFI_Frozen(u1: Ur(), u2: Ur())
}

@_alwaysEmitIntoClient
public func call_piecewise_Agg_Internal_UFI_Frozen() {
  let a = get_Agg_Internal_UFI_Frozen()
  piecewise_Agg_Internal_UFI_Frozen(a)
}

// =====END================== Agg_Internal_UFI_Frozen ========================}}

//--- Downstream.swift

import Library

func piecewise_Agg_Public(_ a: consuming Agg_Public) {
  take(a.u1) // expected-fragile-error{{cannot partially consume 'a' of non-frozen type 'Agg_Public' imported from 'Library'}}
             // expected-resilient-error@-1{{field 'a.u1' was consumed but not reinitialized; the field must be reinitialized during the access}}
             // expected-resilient-note@-2{{consumed here}}
  take(a.u2) // expected-fragile-error{{cannot partially consume 'a' of non-frozen type 'Agg_Public' imported from 'Library'}}
             // expected-resilient-error@-1{{field 'a.u2' was consumed but not reinitialized; the field must be reinitialized during the access}}
             // expected-resilient-note@-2{{consumed here}}
}

func piecewise_Agg_Public_Frozen(_ a: consuming Agg_Public_Frozen) {
  take(a.u1)
  take(a.u2)
}

func piecewise_Agg_Package(_ a: consuming Agg_Package) {
  take(a.u1) // expected-fragile-error{{cannot partially consume 'a' of non-frozen type 'Agg_Package' imported from 'Library'}}
             // expected-resilient-error@-1{{field 'a.u1' was consumed but not reinitialized; the field must be reinitialized during the access}}
             // expected-resilient-note@-2{{consumed here}}
  take(a.u2) // expected-fragile-error{{cannot partially consume 'a' of non-frozen type 'Agg_Package' imported from 'Library'}}
             // expected-resilient-error@-1{{field 'a.u2' was consumed but not reinitialized; the field must be reinitialized during the access}}
             // expected-resilient-note@-2{{consumed here}}
}

// TODO: Uncomment when package decls can be marked @frozen.
// func piecewise_Agg_Package_Frozen(_ a: consuming Agg_Package_Frozen) {
//   take(a.u1)
//   take(a.u2)
// }

func call_call_piecewise_Agg_Internal_UFI_Frozen() {
  call_piecewise_Agg_Internal_UFI_Frozen()
}
