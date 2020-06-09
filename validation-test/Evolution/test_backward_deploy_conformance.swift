// RUN: %target-resilience-test --backward-deployment
// REQUIRES: executable_test

import StdlibUnittest
import backward_deploy_conformance

var BackwardDeployConformanceTest = TestSuite("BackwardDeployConformance")

public class UsesNewStruct: CustomStringConvertible {
  public var field: NewStruct<Bool>? = nil
  public let description = "This is my description"
}

public class OtherClass {}

@_optimize(none)
func blackHole<T>(_: T) {}

BackwardDeployConformanceTest.test("ConformanceCache") {
  if getVersion() == 1 {
    blackHole(UsesNewStruct())
  }

  blackHole(OtherClass() as? CustomStringConvertible)
}

runAllTests()
