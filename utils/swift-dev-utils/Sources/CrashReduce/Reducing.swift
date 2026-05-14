//===--- Reducing.swift ---------------------------------------------------===//
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

extension ProcessReproducers {
  private func withCReduceTest<T>(
    _ options: Reproducer.Options, signatures: KnownSignatures?,
    // FIXME: Shouldn't need @Sendable
    _ body: @Sendable (AbsolutePath) async throws -> T
  ) async throws -> T {
    try await withTemporaryFile { scriptPath in
      try await withTemporaryFile { jsonPath in
        let args = CReduceStep.Args(
          options: options, toolchain: toolchain, signatures: signatures
        )
        try jsonPath.write(JSONEncoder().encode(args))

        let script = """
          #!/bin/sh
          exec \(executablePath.rawPath.escaped) creduce-step --args-json-path \
          \(jsonPath.rawPath.escaped)
          """

        try scriptPath.write(script)
        try scriptPath.chmod(.executable)
        return try await body(scriptPath)
      }
    }
  }

  /// Run a cleanup action on a given crasher. This is done in a child process
  /// since we may have a swift-syntax crasher.
  private func cleanup(
    _ crasher: PotentialCrasher, kind: CleanupKind
  ) async throws -> PotentialCrasher {
    var crasher = crasher
    try await crasher.withInputFiles { inputs in
      for (idx, input) in inputs.enumerated() {
        let result = try await run(
          .path(executablePath.storage), arguments: [
            "cleanup", "--kind", kind.rawValue, input.rawPath
          ],
          environment: .custom([:]),
          output: .string(limit: .max)
        )
        guard result.terminationStatus.isSuccess else {
          throw ReproducerError("failed to cleanup \(input.fileName)")
        }
        crasher.buffers[idx].code = Code(
          (result.standardOutput ?? "").trimmingCharacters(in: .whitespacesAndNewlines)
        )
      }
    }
    return crasher
  }

  private func creduceCrasher(
    _ crasher: PotentialCrasher, signatures: KnownSignatures?
  ) async throws -> PotentialCrasher {
    guard let creducePath = try? Executable.name("creduce")
      .resolveExecutablePath(in: .inherit)
    else {
      log.warning("creduce not found, will not reduce crasher")
      return crasher
    }
    return try await crasher.withInputFiles { inputs in
      try await withCReduceTest(crasher.options, signatures: signatures) { testPath in
        let result = try await run(
          .path(creducePath), arguments: .init(
            [
              "--tidy",
              "--n", "\(ProcessInfo.processInfo.processorCount)",
              "--not-c", testPath.rawPath
            ] + inputs.map(\.rawPath)
          ),
          workingDirectory: testPath.parentDir!.storage,
          output: .string(limit: .max),
          error: .string(limit: .max),
        )
        guard result.terminationStatus.isSuccess else {
          throw ReproducerError("""
          \(result.standardOutput ?? "")
          \(result.standardError ?? "")

          creduce failed for \(crasher)

          """)
        }
      }
      // Write back the reduced buffers.
      var crasher = crasher
      for (idx, input) in inputs.enumerated() {
        crasher.buffers[idx].code = try Code(from: input)
      }
      return crasher
    }
  }

  /// Given a non-deterministic crasher, try some different options to see if
  /// they can make it deterministic.
  func makeDeterministicIfNeeded(_ crasher: Crasher) async throws -> Crasher {
    if crasher.input.options.isDeterministic {
      return crasher
    }
    log.info("checking determinism: \(crasher)".withColor(.gray))

    let input = crasher.input
    let options = input.options

    // Double check that the reproducer is non-deterministic, if it's already
    // deterministic then we're done.
    if let crasher = try await checkDeterministicCrash(of: input) {
      return crasher
    }
    // swift-ide-test runs a random permutation of the completion tokens when
    // run, sometimes that can cause non-determinism. In that case, enforce
    // source order completion.
    // TODO: Ideally we'd use the exact seed expected.
    if options.kind == .complete, !options.useSourceOrderCompletion {
      log.info("\(input) is non-deterministic, trying source order...")
      if let crasher = try await checkDeterministicCrash(
        of: input.withSourceOrderCompletion(true)
      ) {
        return crasher
      }
    }
    // Guard malloc can sometimes help a crasher become deterministic.
    if !options.useGuardMalloc {
      log.info("\(input) is non-deterministic, trying guard malloc...")
      if let crasher = try await checkDeterministicCrash(of: input.withGuardMalloc) {
        return crasher
      }
    }
    log.info("\(input) is non-deterministic")
    return crasher.withDeterministic(false)
  }

  private func reduceCrash(
    of crasher: PotentialCrasher, signatures: KnownSignatures?
  ) async throws -> Reproducer {
    // First try run creduce.
    var crasher = try await creduceCrasher(crasher, signatures: signatures)

    // Make sure we've stripped trivia since e.g RUN lines can interfere
    // with the resulting test case.
    for idx in crasher.buffers.indices {
      crasher.buffers[idx].code = crasher.buffers[idx].code
        .cleanupTrivia(includingWhitespace: true)
    }

    // Try cleanup the reproducer.
    for kind: CleanupKind in [.basicFormat, .fix, .swiftFormat] {
      guard
        let newCrasher = try? await cleanup(crasher, kind: kind),
        let _ = try? await checkCrash(
          of: newCrasher, matchingSignatures: signatures
        )
      else {
        continue
      }
      crasher = newCrasher
    }

    // If we have a multi-file crasher, try join it into a single file.
    if crasher.buffers.count > 1 {
      var joinedCrasher = crasher
      joinedCrasher.buffers = [
        .init(
          name: crasher.buffers[0].name,
          code: Code(
            crasher.buffers.map { $0.code.text }.joined(separator: "\n")
          )
        )
      ]
      if let _ = try? await checkCrash(
        of: joinedCrasher, matchingSignatures: signatures
      ) {
        crasher = joinedCrasher
      }
    }

    // Finally do one more trivia cleanup.
    for idx in crasher.buffers.indices {
      crasher.buffers[idx].code = crasher.buffers[idx].code
        .cleanupTrivia(includingWhitespace: false)
    }

    // Now that we've reduced, double check the crasher is still deterministic.
    var determResult: Crasher?
    if crasher.options.isDeterministic {
      determResult = try await checkDeterministicCrash(of: crasher)
      if determResult == nil {
        log.info("\(crasher) became non-deterministic")
        crasher = crasher.withDeterministic(false)
      }
    }

    // And double check it still actually crashes.
    guard let result = try await checkCrash(
      of: crasher, matchingSignatures: signatures
    ) else {
      guard let signatures,
            let foundCrasher = try await checkCrash(of: crasher)
      else {
        throw ReproducerError(
          "\(crasher) doesn't reproduce after reducing?"
        )
      }
      throw ReproducerError(
            """
            reduced signature \(foundCrasher.primarySig) \
            doesn't match \(signatures.primary)
            """
      )
    }
    // Prefer the deterministic result if we have it since it will likely
    // include more signatures if there are multiple.
    return Reproducer(determResult ?? result)
  }

  private func reduceImpl(_ crasher: Crasher) async throws -> [Reproducer] {
    let signatures = crasher.signatures
    let input = crasher.input

    guard firstNewSignature(signatures) != nil else {
      return []
    }

    log.info("reducing \(crasher)")

    let reduced = try await reduceCrash(of: input, signatures: nil)

    var reproducers: [Reproducer] = []
    reproducers.append(reduced)

    // Sometimes when reducing we go too far end up with a different signature,
    // try again matching the exact signature. We do it this way to potentially
    // get a free extra crasher from the reduction.
    if !signatures.isSuperset(of: reduced.signatures) {
      log.info(
        """
        reduced \(input) to \(reduced.primarySig.shortDescription), \
        reducing preserving signature...
        """
      )
      reproducers.append(try await reduceCrash(of: input, signatures: signatures))
    }
    return reproducers
  }

  private func reduceExtraArgs(_ crasher: Crasher) async throws -> Crasher {
    guard !crasher.input.options.extraArgs.isEmpty else { return crasher }

    // First try without any extra args.
    if let newCrasher = try await checkCrash(of: crasher.input.withExtraArgs([])) {
      return newCrasher
    }

    func removeExtraArgsStep(_ crasher: Crasher, n: Int) async throws -> Crasher {
      var crasher = crasher
      outer: while
        case let extraArgs = crasher.input.options.extraArgs,
        !extraArgs.isEmpty
      {
        for i in extraArgs.indices.dropLast(n - 1) {
          var newExtraArgs = extraArgs
          newExtraArgs.replaceSubrange(i ..< i + n, with: [])
          if let newCrasher = try await checkCrash(
            of: crasher.input.withExtraArgs(newExtraArgs)
          ) {
            crasher = newCrasher
            continue outer
          }
        }
        break
      }
      return crasher
    }

    // First try removing pairs of arguments, then singular args.
    var crasher = crasher
    crasher = try await removeExtraArgsStep(crasher, n: 2)
    crasher = try await removeExtraArgsStep(crasher, n: 1)
    return crasher
  }

  private func reduceCrasherArgs(_ crasher: Crasher) async throws -> Crasher {
    var crasher = crasher

    // Check to see if we can drop the solver limits.
    if crasher.input.options.withSolverLimits {
      if let newCrasher = try await checkCrash(
        of: crasher.input.withSolverLimits(false)
      ) {
        crasher = newCrasher
      }
    }

    // Check to see if can reduce the extra args.
    return try await reduceExtraArgs(crasher)
  }

  func reduce(_ crasher: Crasher) async throws -> [Reproducer] {
    var reproducers: [Reproducer] = []

    // Check to see if we reduce the args.
    let crasher = try await reduceCrasherArgs(crasher)

    let determCrasher = try await makeDeterministicIfNeeded(crasher)
    if determCrasher.input.options != crasher.input.options,
      !determCrasher.signatures.isSuperset(of: crasher.signatures) {
      log.info("""
        determinism changed signature for \(crasher.input) from \
        \(crasher.primarySig.shortDescription) to \
        \(determCrasher.primarySig.shortDescription)
        """)
      reproducers += try await reduceImpl(crasher)
    }
    reproducers += try await reduceImpl(determCrasher)
    return reproducers
  }
}
