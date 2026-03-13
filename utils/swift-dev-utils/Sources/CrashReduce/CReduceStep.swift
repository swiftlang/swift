//===--- CReduceStep.swift ------------------------------------------------===//
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

public import Foundation

public struct CReduceStep {
  var args: Args
  var files: [AbsolutePath]

  init(args: Args, files: [AbsolutePath]) {
    self.args = args
    self.files = files
  }

  struct Args: Codable {
    var options: Reproducer.Options
    var toolchain: Toolchain
    var signatures: KnownSignatures?
  }

  public static func check(
    argsJSON: Data, files: [AbsolutePath]
  ) async throws -> Bool {
    let args = try JSONDecoder().decode(Args.self, from: argsJSON)
    let files = files.sorted { lhs, rhs in
      // FIXME: Egregious hack.
      switch (lhs.fileName, rhs.fileName) {
      case (_, "main.swift"):
        return false
      case ("main.swift", _):
        return true
      case let (lhs, rhs):
        return lhs < rhs
      }
    }
    return try await CReduceStep(args: args, files: files).check()
  }

  func check() async throws -> Bool {
    let crash = try await args.toolchain.checkCrash(
      of: files, options: args.options, matchingSignatures: args.signatures
    )
    return crash != nil
  }
}
