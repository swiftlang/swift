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

// Test lifetimes of captured packs
func lifetimeTest1() -> () -> Any.Type {
  return hasMetadataPack("hello", Set<Int>())
}

func lifetimeTest2() -> () -> Any.Type {
  return hasMetadataPack(3, 1.0)
}

types.test("Lifetime") {
  let fn1 = lifetimeTest1()
  let fn2 = lifetimeTest2()
  expectEqual((String, Set<Int>).self, fn1())
  expectEqual((Int, Double).self, fn2())
}

runAllTests()