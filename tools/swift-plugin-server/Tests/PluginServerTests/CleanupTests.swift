//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if SWIFT_PLUGIN_SERVER_SUPPORTS_WASM

@testable import swift_plugin_server
import SystemPackage
import Testing

@Suite
struct CleanupTests {
  @Test
  func runAllRethrowsSingleFailure() {
    var ran: [Int] = []

    #expect(throws: Errno.badFileDescriptor) {
      try Cleanup.runAll([
        { ran.append(1) },
        {
          ran.append(2)
          throw Errno.badFileDescriptor
        },
        { ran.append(3) },
      ])
    }

    #expect(ran == [1, 2, 3])
  }

  @Test
  func runAllCombinesSeveralFailures() {
    let error = #expect(throws: WasmEngineError.self) {
      try Cleanup.runAll([
        { throw WasmEngineError(message: "first failed") },
        { throw WasmEngineError(message: "second failed") },
      ])
    }

    #expect(error?.description == "first failed; second failed")
  }

  @Test
  func throwAfterRunningReportsCleanupFailure() {
    let error = #expect(throws: WasmEngineError.self) {
      try Cleanup.throwAfterRunning(
        { throw WasmEngineError(message: "close failed") },
        error: WasmEngineError(message: "start failed")
      )
    }

    #expect(error?.description == "start failed; additionally, cleaning up failed: close failed")
  }
}

#endif // SWIFT_PLUGIN_SERVER_SUPPORTS_WASM
