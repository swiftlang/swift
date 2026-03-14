// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -strict-concurrency=complete

// REQUIRES: executable_test
// REQUIRES: concurrency

import StdlibUnittest

var CaseIterableTests = TestSuite("CaseIterableTests")

CaseIterableTests.test("MainActor Isolated Enums") {
  @MainActor
  enum EnumMainActor: CaseIterable {
    case a, b
  }

  expectEqual(EnumMainActor.allCases.count, 2)
  expectEqual(EnumMainActor.allCases, [.a, .b])
}

runAllTests()
