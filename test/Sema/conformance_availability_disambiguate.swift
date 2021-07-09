// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s

// REQUIRES: OS=macosx

public protocol Potato {
  func eat()
}

public protocol Fried {}

extension Potato {
  public func eat() {}
}

@available(macOS 100, *)
extension Potato where Self : Fried {
  public func eat() {}
}

// We ought to pick the unconstrained Potato.eat(), not
// the one from the extension, because the extension has
// narrower availability than the conformance.
public struct CurlyFries : Potato, Fried {}

public struct TaterTots {}

// This conformance on the other hand should use the
// constrained Potato.eat(), since the generic signature
// is more specific than the unconstrained protocol
// extension.
@available(macOS 100, *)
extension TaterTots : Potato, Fried {}

// We FileCheck the SILGen output to verify that the correct
// witnesses were chosen above.

// CHECK-LABEL: sil shared [transparent] [thunk] @$s37conformance_availability_disambiguate10CurlyFriesVAA6PotatoA2aDP3eatyyFTW : $@convention(witness_method: Potato) (@in_guaranteed CurlyFries) -> () {
// CHECK: function_ref @$s37conformance_availability_disambiguate6PotatoPAAE3eatyyF : $@convention(method) <τ_0_0 where τ_0_0 : Potato> (@in_guaranteed τ_0_0) -> ()
// CHECK: return

// sil shared [transparent] [thunk] @$s37conformance_availability_disambiguate9TaterTotsVAA6PotatoA2aDP3eatyyFTW : $@convention(witness_method: Potato) (@in_guaranteed TaterTots) -> () {
// CHECK: function_ref @$s37conformance_availability_disambiguate6PotatoPA2A5FriedRzrlE3eatyyF : $@convention(method) <τ_0_0 where τ_0_0 : Fried, τ_0_0 : Potato> (@in_guaranteed τ_0_0) -> ()
// CHECK: return