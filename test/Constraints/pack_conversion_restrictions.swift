// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple -swift-version 5 -solver-disable-diagnose-valid-salvage
// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple -swift-version 6 -solver-disable-diagnose-valid-salvage

// These @preconcurrency matches currently find valid solutions during salvage.
// Check that applying those solutions preserves the deep-equality conversion.
struct InvariantValue<T> {}
struct PackShape<each T> {}

@preconcurrency func acceptInvariantPack<each T>(
  _ values: (repeat InvariantValue<each T>),
  _ shape: PackShape<repeat each T>
) {}

@preconcurrency func materializeInvariantPack<each T>(
  _ shape: PackShape<repeat each T>
) -> (repeat InvariantValue<each T>) {
  fatalError()
}

func preserveDeepEquality(_ value: InvariantValue<any Sendable>) {
  acceptInvariantPack(value, PackShape<Any>())
  let _: InvariantValue<Any> = materializeInvariantPack(PackShape<any Sendable>())
}
