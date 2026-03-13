// RUN: %empty-directory(%t)
//
// RUN: %target-build-swift-dylib(%t/%target-library-name(variadic_generic_opaque_type_other)) %S/Inputs/variadic_generic_opaque_type_other.swift -emit-module -emit-module-path %t/variadic_generic_opaque_type_other.swiftmodule -module-name variadic_generic_opaque_type_other -target %target-swift-5.9-abi-triple -enable-library-evolution
// RUN: %target-codesign %t/%target-library-name(variadic_generic_opaque_type_other)
//
// RUN: %target-build-swift %s -I %t -o %t/main.out -L %t %target-rpath(%t) -lvariadic_generic_opaque_type_other
// RUN: %target-codesign %t/main.out
//
// RUN: %target-run %t/main.out %t/%target-library-name(variadic_generic_opaque_type_other)

// REQUIRES: executable_test

// This test needs a Swift 5.9 runtime or newer.
// UNSUPPORTED: back_deployment_runtime

import variadic_generic_opaque_type_other
import StdlibUnittest

var opaque = TestSuite("VariadicGenericOpaqueTypes")

func getType<T>(_: T) -> Any.Type {
  return T.self
}

opaque.test("Opaque1") {
  expectEqual((Int, String).self, getType(f1(1, "hi")))
  expectEqual(G2<(Bool, String)>.self, getType(f2(false, "hi")))
  expectEqual(G3<Bool, Double>.self, getType(f3(false, 3.0)))
}

func g1<each T: P>(_ t: repeat each T) -> Any.Type {
  return getType(f1(repeat each t))
}
func g2<each T: P>(_ t: repeat each T) -> Any.Type {
  return getType(f2(repeat each t))
}
func g3<each T: P>(_ t: repeat each T) -> Any.Type {
  return getType(f3(repeat each t))
}

opaque.test("Opaque2") {
  expectEqual((Int, String).self, g1(1, "hi"))
  expectEqual(G2<(Bool, String)>.self, g2(false, "hi"))
  expectEqual(G3<Bool, Double>.self, g3(false, 3.0))
}

runAllTests()
