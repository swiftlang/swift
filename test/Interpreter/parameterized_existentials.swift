// RUN: %target-run-simple-swift(-Xfrontend -enable-parameterized-existential-types)
// REQUIRES: executable_test

import StdlibUnittest

var ParameterizedProtocolsTestSuite = TestSuite("ParameterizedProtocols")

protocol Holder<T> {
  associatedtype T
  var value: T { get }
}

struct IntHolder: Holder {
  var value: Int
}

struct GenericHolder<T>: Holder {
  var value: T
}

ParameterizedProtocolsTestSuite.test("basic") {
  let x: any Holder<Int> = IntHolder(value: 5)
  expectEqual(5, x.value)
}

func staticType<T>(of value: inout T) -> Any.Type {
  return T.self
}

func staticTypeForHolders<T>(of value: inout any Holder<T>) -> Any.Type {
  return (any Holder<T>).self
}

ParameterizedProtocolsTestSuite.test("metadataEquality") {
  var x: any Holder<Int> = IntHolder(value: 5)
  var typeOne = staticType(of: &x)
  var typeTwo = staticTypeForHolders(of: &x)
  expectEqual(typeOne, typeTwo)
}

runAllTests()
