// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking)
// REQUIRES: executable_test

// This test requires the new existential shape metadata accessors which are 
// not available in on-device runtimes, or in the back-deployment runtime.
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

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

ParameterizedProtocolsTestSuite.test("casting") {
  let a = GenericHolder(value: 5) as any Holder<Int>
  let b = GenericHolder(value: 5) as! any Holder<Int>
  expectEqual(a.value, b.value)
}

runAllTests()

