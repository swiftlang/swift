//===--- Toolchain.swift --------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Foundation
import Subprocess

public struct Toolchain: Sendable, Codable {
  var swiftPath: AbsolutePath
  var swiftIDETestPath: AbsolutePath
  var sdkPath: AbsolutePath

  public init(swiftPath: AbsolutePath, sdkPath: AbsolutePath) {
    self.swiftPath = swiftPath
    self.swiftIDETestPath = swiftPath.parentDir!.appending("swift-ide-test")
    self.sdkPath = sdkPath
  }
}

extension Toolchain {
  func checkSingleCrash(
    of inputs: [AbsolutePath], options: Reproducer.Options
  ) async throws -> CrashLog? {
    let result = try await run(
      options.getCommandInvocation(for: inputs, with: self),
      output: .discarded,
      error: .string(limit: .max)
    )
    switch result.terminationStatus {
    case .exited(code: 0), .exited(code: 1):
      return nil
    default:
      let output = result.standardError ?? ""
      guard let crashLog = CrashLog(from: output) else {
        throw ReproducerError("""
          couldn't extract sig for \
          \(inputs.first!.parentDir!.fileName) \
          <sig>\(output)</sig>
          """)
      }
      return crashLog
    }
  }

  private static let determinismIterations = 100

  func checkCrash(
    of inputs: [AbsolutePath], options: Reproducer.Options,
    matchingSignatures signatures: KnownSignatures? = nil
  ) async throws -> CrashLog? {
    @Sendable func checkOnce() async throws -> CrashLog? {
      guard let crash = try await checkSingleCrash(of: inputs, options: options),
            signatures == nil || signatures!.contains(crash.signature) else {
        return nil
      }
      return crash
    }
    if let crash = try await checkOnce() {
      return crash
    }
    if options.isDeterministic {
      return nil
    }
    let worklist = TaskWorklist<CrashLog?>(maxParallel: 6)
    for _ in 1 ..< Self.determinismIterations {
      worklist.addTask {
        try? await checkOnce()
      }
    }
    for await result in worklist.results {
      if let result {
        return result
      }
    }
    return nil
  }

  func checkDeterministicCrash(
    of inputs: [AbsolutePath], options: Reproducer.Options
  ) async throws -> CrashInfo? {
    guard let initialCrash = try await checkSingleCrash(
      of: inputs, options: options
    ) else {
      return nil
    }
    let worklist = TaskWorklist<CrashLog?>(
      maxParallel: ProcessInfo.processInfo.processorCount
    )
    for _ in 1 ..< Self.determinismIterations {
      worklist.addTask {
        try? await checkSingleCrash(of: inputs, options: options)
      }
    }
    var crashInfo = CrashInfo(initialCrash)
    for await crash in worklist.results {
      guard let crash else {
        return nil
      }
      crashInfo.add(crash)
    }
    return crashInfo
  }
}
