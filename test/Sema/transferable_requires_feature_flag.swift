// RUN: %target-typecheck-verify-swift -verify-additional-prefix disabled-
// RUN: %target-typecheck-verify-swift -enable-experimental-feature NontransferableTypes

// REQUIRES: swift_feature_NontransferableTypes

// Suppressing or naming Transferable requires NontransferableTypes flag being enabled.

struct S: ~Transferable {}
// expected-disabled-error@-1 {{'Transferable' requires '-enable-experimental-feature NontransferableTypes'}}
enum E: ~Transferable {}
// expected-disabled-error@-1 {{'Transferable' requires '-enable-experimental-feature NontransferableTypes'}}
class C: ~Transferable {}
// expected-disabled-error@-1 {{'Transferable' requires '-enable-experimental-feature NontransferableTypes'}}

func genericParameter<T: ~Transferable>(_: T) {}
// expected-disabled-error@-1 {{'Transferable' requires '-enable-experimental-feature NontransferableTypes'}}
func whereClause<T>(_: T) where T: ~Transferable {}
// expected-disabled-error@-1 {{'Transferable' requires '-enable-experimental-feature NontransferableTypes'}}
func existential(_: any ~Transferable) {}
// expected-disabled-error@-1 {{'Transferable' requires '-enable-experimental-feature NontransferableTypes'}}

protocol P: ~Transferable {}
// expected-disabled-error@-1 {{'Transferable' requires '-enable-experimental-feature NontransferableTypes'}}
// FIXME: Error recovery for an invalid inverse shouldn't produce this.
// expected-disabled-warning@-3 {{protocol 'P' should be declared to refine 'Transferable' due to a same-type constraint on 'Self'}}
protocol HasAssoc {
  associatedtype A: ~Transferable
  // expected-disabled-error@-1 {{'Transferable' requires '-enable-experimental-feature NontransferableTypes'}}
}

struct ExplicitlyTransferable: Transferable {}
// expected-disabled-error@-1 {{'Transferable' requires '-enable-experimental-feature NontransferableTypes'}}
func requiresTransferable<T: Transferable>(_: T) {}
// expected-disabled-error@-1 {{'Transferable' requires '-enable-experimental-feature NontransferableTypes'}}
func qualified<T: Swift.Transferable>(_: T) {}
// expected-disabled-error@-1 {{'Transferable' requires '-enable-experimental-feature NontransferableTypes'}}
func positiveExistential(_: any Transferable) {}
// expected-disabled-error@-1 {{'Transferable' requires '-enable-experimental-feature NontransferableTypes'}}
