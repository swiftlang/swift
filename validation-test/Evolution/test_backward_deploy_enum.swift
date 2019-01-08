// RUN: %target-resilience-test --backward-deployment
// REQUIRES: executable_test

import StdlibUnittest
import backward_deploy_enum

// <rdar://problem/46438568>
// XFAIL: *

var BackwardDeployEnumTest = TestSuite("BackwardDeployEnum")

func checkIt(_ e: ResilientEnum) -> Int {
  switch e {
  case .first:
    return 1
  case .second:
    return 2
  case .third:
    return 3
  case .fourth:
    return 4
  default:
    return 5
  }
}

BackwardDeployEnumTest.test("ResilientEnum") {

  expectEqual(1, checkIt(.first))
  expectEqual(2, checkIt(.second))
  expectEqual(4, checkIt(.fourth))
  expectEqual(5, checkIt(.fifth))

  if getVersion() == 1 {
    expectEqual(3, checkIt(.third))
  }
}

runAllTests()
