// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/newSDK %target-link-sdk-future-version
// RUN: %target-codesign %t/newSDK
// RUN: %target-run %t/newSDK newSDK
// RUN: %target-build-swift %s -o %t/oldSDK %target-link-sdk-2020-version
// RUN: %target-codesign %t/oldSDK
// RUN: %target-run %t/oldSDK oldSDK

// REQUIRES: VENDOR=apple
// REQUIRES: executable_test

// Simulators refuse to run binaries built with an SDK newer than the simulator.
// UNSUPPORTED: DARWIN_SIMULATOR=ios
// UNSUPPORTED: DARWIN_SIMULATOR=tvos
// UNSUPPORTED: DARWIN_SIMULATOR=watchos

// UNSUPPORTED: use_os_stdlib

import Accelerate
import Foundation
import StdlibUnittest
import SwiftShims


extension CFString: Hashable {
  static var localHashableCallCount = 0
  public var hashValue: Int {
    Self.localHashableCallCount += 1
    return (self as String).hashValue
  }
}

protocol P {
  func firstHashValue() -> Int
}

extension Set: P {
  func firstHashValue() -> Int {
    return first!.hashValue
  }
}

@_optimize(none)
func firstHashValue(_ x: P) -> Int {
  x.firstHashValue()
}

let osHasWorkaround: Bool
if #available(macOS 11.3, iOS 14.5, tvOS 14.5, watchOS 7.4, *) {
  osHasWorkaround = true
} else {
  osHasWorkaround = false
}

let testingOldSDK = CommandLine.arguments.last == "oldSDK"

var tests: TestSuite

if testingOldSDK {
  tests = TestSuite("old SDK protocol conformance collision")
  tests.test("CFString: Hashable conformance") {
    _ = firstHashValue(NSSet(object: "Whatever") as! Set<CFString>)

    let expectedCallCount = osHasWorkaround ? 1 : 0
    expectEqual(expectedCallCount, CFString.localHashableCallCount)
  }
} else {
  tests = TestSuite("new SDK protocol conformance collision")
  tests.test("CFString: Hashable conformance") {
    _ = firstHashValue(NSSet(object: "Whatever") as! Set<CFString>)

    expectEqual(0, CFString.localHashableCallCount)
  }
}

runAllTests()
