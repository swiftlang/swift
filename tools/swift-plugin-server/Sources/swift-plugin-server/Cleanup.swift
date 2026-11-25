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

enum Cleanup {
  /// Calls every closure in `actions`, including the ones after a closure that throws.
  /// Rethrows the error when one closure fails, and throws one error describing every
  /// failure when several do.
  static func runAll(_ actions: [() throws -> Void]) throws {
    var errors: [any Error] = []
    for action in actions {
      do {
        try action()
      } catch {
        errors.append(error)
      }
    }
    if errors.count == 1 {
      throw errors[0]
    }
    if !errors.isEmpty {
      throw WasmEngineError(message: errors.map { "\($0)" }.joined(separator: "; "))
    }
  }

  /// Runs `cleanup`, then throws `error`, combined with the cleanup's error when it fails.
  static func throwAfterRunning(_ cleanup: () throws -> Void, error: any Error) throws -> Never {
    do {
      try cleanup()
    } catch let cleanupError {
      throw WasmEngineError(message: "\(error); additionally, cleaning up failed: \(cleanupError)")
    }
    throw error
  }
}

#endif // SWIFT_PLUGIN_SERVER_SUPPORTS_WASM
