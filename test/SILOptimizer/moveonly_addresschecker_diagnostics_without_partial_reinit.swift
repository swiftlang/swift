// RUN: %target-swift-emit-sil -O -sil-verify-all -verify -enable-experimental-feature NoImplicitCopy -enable-experimental-feature MoveOnlyClasses %s

// REQUIRES: swift_feature_MoveOnlyClasses
// REQUIRES: swift_feature_NoImplicitCopy

// Test diagnostics for partial-consumption without partial-reinitialization.

struct Ur : ~Copyable {
  deinit {}
}

struct Agg : ~Copyable {
  var u1: Ur
  var u2: Ur
}

func borrow(_ u1: borrowing Ur) {}
func consume(_ u1: consuming Ur) {}
func consume(_ u1: consuming Agg) {}
func rewrite(_ u: inout Ur) {}

// First consumes and then reinits each field.
func reinit_1(agg: inout Agg, u1: consuming Ur, u2: consuming Ur) {
  consume(agg.u1)
  consume(agg.u2)
  agg.u1 = u1 // expected-error{{cannot partially reinitialize 'agg' after it has been consumed; only full reinitialization is allowed}}
              // expected-note@-3{{consumed here}}
  agg.u2 = u2 // expected-error{{cannot partially reinitialize 'agg' after it has been consumed; only full reinitialization is allowed}}
              // expected-note@-4{{consumed here}}
}

// Just replace fields of inout argument, no reinitialization.
func not_reinit_1(agg: inout Agg, u1: consuming Ur, u2: consuming Ur) {
  agg.u1 = u1
  agg.u2 = u2
}

// Full reinitialization is legal.
func not_reinit_2(agg: inout Agg, agg2: consuming Agg) {
  consume(agg)
  agg = agg2
}

// Passing fields as inout is allowed.
func not_reinit_3(agg: inout Agg) {
  rewrite(&agg.u1)
  rewrite(&agg.u2)
}
