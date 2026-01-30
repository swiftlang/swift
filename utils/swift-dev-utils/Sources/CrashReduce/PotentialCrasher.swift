//===--- PotentialCrasher.swift -------------------------------------------===//
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

/// A potential crasher input with a given set of options to use.
struct PotentialCrasher: Hashable {
  var path: AbsolutePath
  var options: Reproducer.Options
  var buffers: [Buffer]
  var annotatedBuffers: [Buffer]

  init(_ file: ReproducerFile) {
    self.path = file.path
    self.options = file.reproducer.options
    self.buffers = file.reproducer.buffers
    self.annotatedBuffers = buffers
  }

  init(
    path: AbsolutePath, options: Reproducer.Options, buffers: [Buffer],
    annotatedBuffers: [Buffer]? = nil
  ) {
    self.path = path
    self.options = options
    self.buffers = buffers
    self.annotatedBuffers = annotatedBuffers ?? buffers
  }

  init(_ input: FuzzerInput, options: Reproducer.Options) {
    self.init(path: input.path, options: options, buffers: input.buffers)
  }

  static func typecheck(_ input: FuzzerInput) -> Self {
    .init(input, options: .typecheck)
  }

  static func emitSILGen(_ input: FuzzerInput) -> Self {
    .init(input, options: .emitSILGen)
  }

  static func emitSIL(_ input: FuzzerInput) -> Self {
    .init(input, options: .emitSIL)
  }

  static func emitIR(_ input: FuzzerInput) -> Self {
    .init(input, options: .emitIR)
  }

  static func completion(_ input: FuzzerInput) throws -> Self? {
    let reqs = input.header.sourceKitRequests.sorted(by: \.offset)
      .filter { $0.kind == .complete }
    var buffers = input.buffers
    if reqs.isEmpty {
      if buffers.contains(where: { $0.code.text.contains(#"#^"#)} ) {
        return Self(
          path: input.path, options: .complete, buffers: buffers,
          annotatedBuffers: buffers
        )
      }
      return nil
    }
    if reqs.contains(where: { $0.fileIdx >= buffers.count }) {
      throw ReproducerError("""
        completion in \(input) exceeds number of buffers
        """)
    }
    let annotatedBuffers = try buffers.enumerated().map { bufferIdx, buffer in
      var buffer = buffer
      var bytes = Array(buffer.code.text.replacing("#^", with: "# ").utf8)
      var preOffset = 0
      for (i, req) in reqs.enumerated() where req.fileIdx == bufferIdx {
        let tok = "#^COMPLETE\(i + 1)^#".utf8
        let offset = req.offset + preOffset
        guard offset <= bytes.count else {
          throw ReproducerError("""
            completion at \(offset) exceeded \(bytes.count) for \(input)
            """)
        }
        bytes.insert(contentsOf: tok, at: offset)
        preOffset += tok.count
      }
      buffer.code = Code(String(decoding: bytes, as: UTF8.self))
      return buffer
    }
    if buffers.count == 1 {
      buffers = annotatedBuffers
    }
    return Self(
      path: input.path, options: .complete, buffers: buffers,
      annotatedBuffers: annotatedBuffers
    )
  }

  static func custom(
    _ input: FuzzerInput, frontendArgs: [Command.Argument]
  ) -> Self {
    .init(input, options: .custom(frontendArgs: frontendArgs))
  }

  var withGuardMalloc: Self {
    var result = self
    result.options.useGuardMalloc = true
    return result
  }

  func withSourceOrderCompletion(_ value: Bool = true) -> Self {
    var result = self
    result.options.useSourceOrderCompletion = value
    return result
  }

  var withNoSDK: Self {
    var result = self
    result.options.noSDK = true
    return result
  }

  var withNoObjCInterop: Self {
    var result = self
    result.options.noObjCInterop = true
    return result
  }

  func withSolverLimits(_ value: Bool = true) -> Self {
    var result = self
    result.options.withSolverLimits = value
    return result
  }

  var hasSolverLimits: Bool {
    options.withSolverLimits
  }

  func withLanguageMode(_ value: Int?) -> Self {
    var result = self
    result.options.languageMode = value
    return result
  }

  func withDeterministic(_ value: Bool = true) -> Self {
    var result = self
    result.options.isDeterministic = value
    return result
  }

  func withPrimaryIdx(_ idx: Int) -> Self {
    var result = self
    result.options.primaryIdx = idx
    result.buffers[idx] = result.annotatedBuffers[idx]
    return result
  }

  func withJoinedBuffers() -> Self {
    var result = self
    result.options.isJoined = true
    result.options.primaryIdx = nil
    let code = Code(annotatedBuffers.map { $0.code.text }.joined(separator: "\n"))
    result.buffers = [Buffer(name: annotatedBuffers[0].name, code: code)]
    result.annotatedBuffers = result.buffers
    return result
  }

  private static let idRegex = #/crash-([a-fA-F0-9]{8})/#

  var id: String? {
    guard let match = path.fileName.firstMatch(of: Self.idRegex) else {
      return nil
    }
    return String(match.output.1)
  }
}

extension PotentialCrasher: CustomStringConvertible {
  var description: String {
    "(\(options)) \(path.fileName)"
  }
}

extension PotentialCrasher {
  func withInputFiles<T>(
    _ body: ([AbsolutePath]) async throws -> T
  ) async throws -> T {
    try await withTemporaryDirectory(prefix: path.fileName) { dir in
      var files: [AbsolutePath] = []
      for buffer in buffers {
        let file = dir.appending(buffer.name)
        files.append(file)
        try file.write(buffer.code.text)
      }
      return try await body(files)
    }
  }
}

struct Crasher {
  var input: PotentialCrasher
  var crashInfo: CrashInfo

  var primaryCrash: CrashLog {
    crashInfo.primary
  }
  var primarySig: Signature {
    crashInfo.primarySig
  }

  var signatures: KnownSignatures { crashInfo.signatures }
  var path: AbsolutePath { input.path }
  var kind: Reproducer.Options { input.options }
  var buffers: [Buffer] { input.buffers }
}

extension Crasher {
  func withDeterministic(_ value: Bool) -> Self {
    Self(input: input.withDeterministic(value), crashInfo: crashInfo)
  }
  func withSourceOrderCompletion(_ value: Bool) -> Self {
    Self(input: input.withSourceOrderCompletion(value), crashInfo: crashInfo)
  }
}

extension Crasher: CustomStringConvertible {
  var description: String {
    "\(signatures) | \(input)"
  }
}
