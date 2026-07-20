// RUN: %target-swift-emit-sil %s -enable-experimental-feature Embedded -wmo -module-name main | %FileCheck %s

// REQUIRES: swift_feature_Embedded

// A non-final class that conforms to a protocol via a protocol-extension
// default implementation gets a witness thunk that is generic over its
// (class-constrained) conforming type -- the covariant-Self pattern,
// `<τ : ConcreteClass>`. The default implementation is itself generic over
// `<Self : Protocol>`, whose signature cannot be emitted in embedded Swift.
//
// The mandatory-performance-optimizations pass must inline that default impl
// into the witness thunk so the thunk no longer references the un-emittable
// generic callee. This test locks in that inlining (and guards against the
// inliner failing to fire, or -- for a recursive default impl -- inlining
// unboundedly and exhausting memory).

public protocol Greeter {
  func greet() -> Int
}

extension Greeter {
  public func greet() -> Int { return 100 }
}

// Non-final class: its witness thunk is generic over `<τ : Animal>`.
public class Animal: Greeter {}

// A subclass exercises the covariant-Self aspect (the thunk really is generic,
// not specialized to the exact class).
public class Dog: Animal {}

// Use the conformers through an existential so the witness table -- and thus
// the generic witness thunk -- is live and reaches the optimizer/IRGen.
@_silgen_name("run")
public func run(_ g: any Greeter) -> Int {
  return g.greet()
}

// The witness thunk for Animal must have the default impl inlined: no
// `function_ref`/`apply` of the generic default implementation remains, just
// its folded-in body returning 100.

// CHECK-LABEL: sil {{.*}}@$e4main6AnimalCAA7GreeterA2aDP5greetSiyFTW : $@convention(witness_method: Greeter) <τ_0_0 where τ_0_0 : Animal>
// CHECK-NOT:     function_ref @$e4main7GreeterPAAE5greetSiyF
// CHECK-NOT:     apply
// CHECK:         integer_literal $Builtin.Int{{[0-9]+}}, 100
// CHECK:       } // end sil function '$e4main6AnimalCAA7GreeterA2aDP5greetSiyFTW'
