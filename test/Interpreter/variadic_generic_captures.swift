// RUN: %target-run-simple-swift(-enable-experimental-feature VariadicGenerics -Xfrontend -disable-concrete-type-metadata-mangled-name-accessors)
// RUN: %target-run-simple-swift(-enable-experimental-feature VariadicGenerics)

// FIXME: Fix the optimizer
// REQUIRES: swift_test_mode_optimize_none

// REQUIRES: executable_test

// Because of -enable-experimental-feature VariadicGenerics
// REQUIRES: asserts

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest

var types = TestSuite("VariadicGenericCaptures")

func hasMetadataPack<each T>(_: repeat each T) -> () -> Any.Type {
  return { return (repeat each T).self }
}

types.test("Metadata") {
  expectEqual(Void.self, hasMetadataPack()())
  expectEqual((Int, String, Bool).self, hasMetadataPack(1, "hi", false)())
}

func hasWitnessTablePack<each T: Sequence>(_: repeat each T) -> () -> Any.Type {
  return { return (repeat (each T).Element).self }
}

types.test("WitnessTable") {
  expectEqual(Void.self, hasWitnessTablePack()())
  expectEqual((Int, String, Bool).self, hasWitnessTablePack([1], ["hi"], [false])())
}

func hasWitnessTablePack2<each T: Sequence>(_: repeat each T) -> () -> Any.Type where (each T).Element: Sequence {
  return { return (repeat (each T).Element.Element).self }
}

types.test("WitnessTable2") {
  expectEqual(Void.self, hasWitnessTablePack2()())
  expectEqual((Int, String, Bool).self, hasWitnessTablePack2([[1]], [["hi"]], [[false]])())
}

runAllTests()