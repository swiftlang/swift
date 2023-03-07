// RUN: %target-run-simple-swift(-enable-experimental-feature VariadicGenerics -Xfrontend -disable-concrete-type-metadata-mangled-name-accessors)

// FIXME: Get it to build without -disable-concrete-type-metadata-mangled-name-accessors and test both

// REQUIRES: executable_test

// Because of -enable-experimental-feature VariadicGenerics
// REQUIRES: asserts

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest

var types = TestSuite("VariadicGenericTypes")

public struct Outer<each U>
    where each U: Equatable {

  public struct Inner<each V>
      where each V: Equatable {
  }

  public struct InnerSameShape<each V>
      where each V: Equatable,
            (repeat (each U, each V)): Any {
  }
}

types.test("Outer") {
  expectEqual("main.Outer<Pack{}>", _typeName(Outer< >.self))
  expectEqual("main.Outer<Pack{Swift.Int}>", _typeName(Outer<Int>.self))
  expectEqual("main.Outer<Pack{Swift.Int, Swift.String}>", _typeName(Outer<Int, String>.self))
  expectEqual("main.Outer<Pack{Swift.Int, Swift.String, Swift.Float}>", _typeName(Outer<Int, String, Float>.self))
}

types.test("Outer.Inner") {
  expectEqual("main.Outer<Pack{}>.Inner<Pack{}>", _typeName(Outer< >.Inner< >.self))
  expectEqual("main.Outer<Pack{Swift.Int}>.Inner<Pack{}>", _typeName(Outer<Int>.Inner< >.self))
  expectEqual("main.Outer<Pack{Swift.Int, Swift.String}>.Inner<Pack{}>", _typeName(Outer<Int, String>.Inner< >.self))
  expectEqual("main.Outer<Pack{Swift.Int, Swift.String, Swift.Float}>.Inner<Pack{}>", _typeName(Outer<Int, String, Float>.Inner< >.self))

  expectEqual("main.Outer<Pack{}>.Inner<Pack{Swift.Bool}>", _typeName(Outer< >.Inner<Bool>.self))
  expectEqual("main.Outer<Pack{Swift.Int}>.Inner<Pack{Swift.Bool}>", _typeName(Outer<Int>.Inner<Bool>.self))
  expectEqual("main.Outer<Pack{Swift.Int, Swift.String}>.Inner<Pack{Swift.Bool}>", _typeName(Outer<Int, String>.Inner<Bool>.self))
  expectEqual("main.Outer<Pack{Swift.Int, Swift.String, Swift.Float}>.Inner<Pack{Swift.Bool}>", _typeName(Outer<Int, String, Float>.Inner<Bool>.self))

  expectEqual("main.Outer<Pack{}>.Inner<Pack{Swift.Bool, Swift.Double}>", _typeName(Outer< >.Inner<Bool, Double>.self))
  expectEqual("main.Outer<Pack{Swift.Int}>.Inner<Pack{Swift.Bool, Swift.Double}>", _typeName(Outer<Int>.Inner<Bool, Double>.self))
  expectEqual("main.Outer<Pack{Swift.Int, Swift.String}>.Inner<Pack{Swift.Bool, Swift.Double}>", _typeName(Outer<Int, String>.Inner<Bool, Double>.self))
  expectEqual("main.Outer<Pack{Swift.Int, Swift.String, Swift.Float}>.Inner<Pack{Swift.Bool, Swift.Double}>", _typeName(Outer<Int, String, Float>.Inner<Bool, Double>.self))

  expectEqual("main.Outer<Pack{}>.Inner<Pack{Swift.Bool, Swift.Double, Swift.Character}>", _typeName(Outer< >.Inner<Bool, Double, Character>.self))
  expectEqual("main.Outer<Pack{Swift.Int}>.Inner<Pack{Swift.Bool, Swift.Double, Swift.Character}>", _typeName(Outer<Int>.Inner<Bool, Double, Character>.self))
  expectEqual("main.Outer<Pack{Swift.Int, Swift.String}>.Inner<Pack{Swift.Bool, Swift.Double, Swift.Character}>", _typeName(Outer<Int, String>.Inner<Bool, Double, Character>.self))
  expectEqual("main.Outer<Pack{Swift.Int, Swift.String, Swift.Float}>.Inner<Pack{Swift.Bool, Swift.Double, Swift.Character}>", _typeName(Outer<Int, String, Float>.Inner<Bool, Double, Character>.self))
}

types.test("Outer.InnerSameShape") {
  expectEqual("main.Outer<Pack{}>.InnerSameShape<Pack{}>", _typeName(Outer< >.InnerSameShape< >.self))
  expectEqual("main.Outer<Pack{Swift.Int}>.InnerSameShape<Pack{Swift.Bool}>", _typeName(Outer<Int>.InnerSameShape<Bool>.self))
  expectEqual("main.Outer<Pack{Swift.Int, Swift.String}>.InnerSameShape<Pack{Swift.Bool, Swift.Double}>", _typeName(Outer<Int, String>.InnerSameShape<Bool, Double>.self))
  expectEqual("main.Outer<Pack{Swift.Int, Swift.String, Swift.Float}>.InnerSameShape<Pack{Swift.Bool, Swift.Double, Swift.Character}>", _typeName(Outer<Int, String, Float>.InnerSameShape<Bool, Double, Character>.self))
}

runAllTests()