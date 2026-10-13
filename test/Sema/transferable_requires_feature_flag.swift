// RUN: %target-typecheck-verify-swift -verify-additional-prefix disabled-
// RUN: %target-typecheck-verify-swift -enable-experimental-feature TransferableProtocol

// REQUIRES: swift_feature_TransferableProtocol

// Suppressing or naming Transferable requires TransferableProtocol flag being enabled.

struct S: ~Transferable {}
// expected-disabled-error@-1 {{'Transferable' requires '-enable-experimental-feature TransferableProtocol'}}
enum E: ~Transferable {}
// expected-disabled-error@-1 {{'Transferable' requires '-enable-experimental-feature TransferableProtocol'}}
class C: ~Transferable {}
// expected-disabled-error@-1 {{'Transferable' requires '-enable-experimental-feature TransferableProtocol'}}

func genericParameter<T: ~Transferable>(_: T) {}
// expected-disabled-error@-1 {{'Transferable' requires '-enable-experimental-feature TransferableProtocol'}}
func whereClause<T>(_: T) where T: ~Transferable {}
// expected-disabled-error@-1 {{'Transferable' requires '-enable-experimental-feature TransferableProtocol'}}
func existential(_: any ~Transferable) {}
// expected-disabled-error@-1 {{'Transferable' requires '-enable-experimental-feature TransferableProtocol'}}

protocol P: ~Transferable {}
// expected-disabled-error@-1 {{'Transferable' requires '-enable-experimental-feature TransferableProtocol'}}
// FIXME: Error recovery for an invalid inverse shouldn't produce this.
// expected-disabled-warning@-3 {{protocol 'P' should be declared to refine 'Transferable' due to a same-type constraint on 'Self'}}
protocol HasAssoc {
  associatedtype A: ~Transferable
  // expected-disabled-error@-1 {{'Transferable' requires '-enable-experimental-feature TransferableProtocol'}}
}

struct ExplicitlyTransferable: Transferable {}
// expected-disabled-error@-1 {{'Transferable' requires '-enable-experimental-feature TransferableProtocol'}}
func requiresTransferable<T: Transferable>(_: T) {}
// expected-disabled-error@-1 {{'Transferable' requires '-enable-experimental-feature TransferableProtocol'}}
func qualified<T: Swift.Transferable>(_: T) {}
// expected-disabled-error@-1 {{'Transferable' requires '-enable-experimental-feature TransferableProtocol'}}
func positiveExistential(_: any Transferable) {}
// expected-disabled-error@-1 {{'Transferable' requires '-enable-experimental-feature TransferableProtocol'}}
