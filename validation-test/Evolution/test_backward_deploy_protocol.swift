// RUN: %target-resilience-test --backward-deployment
// REQUIRES: executable_test

import StdlibUnittest
import backward_deploy_protocol


var BackwardDeployProtocolTest = TestSuite("BackwardDeployProtocol")

struct ConformsToOldWithDefault : OldProtocol {}

struct MyConcrete : OtherProtocol {}
struct ConformsToOldWithNonDefault : OldProtocol {
  typealias NewType = MyConcrete
}

BackwardDeployProtocolTest.test("OldProtocol") {
  if getVersion() == 1 {
    _ = ConformsToOldWithDefault().newMethod()
    _ = ConformsToOldWithNonDefault().newMethod()
  }
}

struct MyNewConforms : NewProtocol {
  func newMethod() {}
}

func dynamicCast<T, U>(_ t: T, _: U.Type) -> Bool {
  return t is U
}

BackwardDeployProtocolTest.test("NewProtocol") {
  if getVersion() == 1 {
    let x1 = NewConforms()
    let x2 = MyNewConforms()
    let x3 = ConformsToOldWithDefault()

    expectEqual(true, dynamicCast(x1, NewProtocol.self))
    expectEqual(true, dynamicCast(x2, NewProtocol.self))
    expectEqual(false, dynamicCast(x3, NewProtocol.self))
  }

  // Make sure that dynamic casts don't crash in the backward
  // deployment case.
  do {
    let x1 = ConformsToOldWithDefault()
    let x2 = MyConcrete()

    expectEqual(false, dynamicCast(x1, OtherProtocol.self))
    expectEqual(true, dynamicCast(x2, OtherProtocol.self))
  }
}

// Weak reference to a conformance descriptor from another module
public protocol RefinedProtocol : NewProtocol {}
extension NewConforms : RefinedProtocol {}

BackwardDeployProtocolTest.test("RefinedProtocol") {
  if getVersion() == 1 {
    let x1 = NewConforms()
    let x2 = MyNewConforms()
    let x3 = ConformsToOldWithDefault()

    expectEqual(true, dynamicCast(x1, RefinedProtocol.self))
    expectEqual(false, dynamicCast(x2, RefinedProtocol.self))
    expectEqual(false, dynamicCast(x3, RefinedProtocol.self))
  }
}

// Conditional conformance with weak-linked requirement
struct Box<T> {}

extension Box : OtherProtocol where T : NewProtocol {}

BackwardDeployProtocolTest.test("ConditionalConformance") {
  if getVersion() == 1 {
    let x1 = Box<MyNewConforms>()

    expectEqual(true, dynamicCast(x1, OtherProtocol.self))
  }

  do {
    let x2 = Box<Int>()
    expectEqual(false, dynamicCast(x2, OtherProtocol.self))
  }
}

runAllTests()
