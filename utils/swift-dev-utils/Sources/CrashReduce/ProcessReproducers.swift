//===--- ProcessReproducers.swift -----------------------------------------===//
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
import Synchronization

public actor ProcessReproducers {
  private var reproducersBySignature: [Signature: ReproducerFile] = [:]

  let toolchain: Toolchain

  let inputDir: AbsolutePath?
  let otherInputs: [AbsolutePath]

  let outputDir: AbsolutePath
  let ideOutputDir: AbsolutePath

  let executablePath: AbsolutePath

  let quickMode: Bool
  let deleteInputs: Bool

  public init(
    from inputDir: AbsolutePath?, to outputDir: AbsolutePath,
    otherInputs: [AbsolutePath], ideOutputDir: AbsolutePath?,
    toolchain: Toolchain, quickMode: Bool, deleteInputs: Bool
  ) throws {
    self.toolchain = toolchain
    self.inputDir = inputDir
    self.otherInputs = otherInputs
    self.outputDir = outputDir
    self.ideOutputDir = ideOutputDir ?? outputDir
    guard let execPath = Bundle.main.executablePath else {
      struct CannotFindExecutableError: Error {}
      throw CannotFindExecutableError()
    }
    self.executablePath = AnyPath(execPath).absoluteInWorkingDir
    self.quickMode = quickMode
    self.deleteInputs = deleteInputs
  }

  func writeReproducer(_ reproducer: Reproducer) throws {
    guard firstNewSignature(reproducer.signatures) != nil else { return }
    let repoFile = ReproducerFile(
      in: reproducer.kind == .complete ? ideOutputDir : outputDir,
      reproducer: reproducer
    )
    recordReproducer(repoFile)
    try repoFile.write()

    // Check the file round-trips
    do {
      let before = try repoFile.path.read()
      let repo = try Reproducer.decoding(before)
      let after = try? repo?.serialize()
      if after != before {
        throw ReproducerError("""
          \(repoFile) didn't round-trip!
          before:
          \(String(decoding: before, as: UTF8.self))
          after:
          \(String(decoding: after ?? Data(), as: UTF8.self))
          <end>
          """)
      }
    }
  }

  func getCrash(for reproFile: ReproducerFile) async throws -> CrashInfo? {
    if let crashLog = reproFile.reproducer.crashInfo {
      return crashLog
    }
    return try await checkCrash(of: PotentialCrasher(reproFile))?.crashInfo
  }

  func firstNewSignature(_ sigs: KnownSignatures) -> Signature? {
    sigs.sigs.sorted().first(where: {
      reproducersBySignature[$0] == nil
    })
  }

  func recordReproducer(_ repro: ReproducerFile) {
    for sig in repro.reproducer.signatures.sigs {
      reproducersBySignature[sig] = repro
    }
  }

  func fileReproIssues() async throws {
    for (_, reproFile) in reproducersBySignature.sorted(by: \.key).prefix(1) {
      var repro: Reproducer {
        _read { yield reproFile.reproducer }
        _modify { yield &reproFile.reproducer }
      }
      guard
        repro.issueID == nil,
        let crashLog = try await getCrash(for: reproFile)?.primary,
        let issue = GitHubIssue(from: repro, crashLog: crashLog)
      else {
        continue
      }
      print("Will file issue:")
      print("Title: \(issue.title)")
      print("Body: <body>\n\(issue.body)\n</body>")
      print("Labels: \(issue.labels.map(\.rawValue))")
      print("Press ENTER to post")
      _ = readLine()
      do {
        let response = try await issue.post(owner: "swiftlang", repoName: "swift")
        log.info("posted \(response.htmlURL)")
        repro.issueID = response.number
        reproFile.path = reproFile.path.parentDir!
          .appending("issue-\(response.number).swift")
        try reproFile.write()
      } catch {
        log.warning("failed to file issue \(error)")
      }
    }
  }

  public func process(
    reprocess: Bool, ignoreExisting: Bool, fileIssues: Bool,
    frontendArgs: [Command.Argument]
  ) async throws {
    // TODO: This function should be refactored...
    let start = Date()
    if !ignoreExisting {
      var reproFiles: [ReproducerFile] = []
      for outputPath in Set([self.outputDir, ideOutputDir]) {
        for file in try outputPath.getDirContents() where file.hasExtension(.swift) {
          let absPath = outputPath.appending(file)
          guard let reproFile = try ReproducerFile(from: absPath) else { continue }
          reproFiles.append(reproFile)
        }
      }
      if reprocess {
        log.info("re-processing reproducers...")
        let worklist = TaskWorklist<Void>()
        for repro in reproFiles {
          worklist.addTask {
            do {
              guard let crash = try await self.getCrash(for: repro) else {
                // FIXME: Workaround https://github.com/swiftlang/swift/issues/86623
                let logMsg = "\(repro) did not re-reproduce"
                log.warning(logMsg)
                return
              }
              let oldSigs = repro.reproducer.signatures
              let oldIsStackOverflow = repro.reproducer.isStackOverflow
              repro.reproducer.signatures = crash.signatures
              repro.reproducer.isStackOverflow = crash.primary.isStackOverflow
              do {
                // FIXME: Workaround https://github.com/swiftlang/swift/issues/86623
                let logMsg = "found: \(repro)"
                log.info(logMsg)
              }
              if oldSigs.primary != crash.signatures.primary ||
                  oldIsStackOverflow != crash.primary.isStackOverflow {
                repro.checkFilename(warn: false)
                try repro.write()
              }
            } catch {
              log.warning("\(error)")
            }
          }
        }
        for await _ in worklist.results {}
      }
      for repro in reproFiles {
        recordReproducer(repro)
      }
    }

    var inputPaths = otherInputs
    if let inputDir {
      inputPaths += try inputDir.getDirContents()
        .map { inputDir.appending($0) }
        .filter { !$0.isDirectory && !$0.isSymlink }
    }

    let count = inputPaths.count
    let progress = Atomic(0)

    log.prefixFn = { output, useColor in
      let progress = progress.load(ordering: .acquiring)
      "[\(progress)/\(count)]".withColor(.gray).write(to: output, useColor: useColor)
    }

    var seenSigs: Set<Signature> = []

    let repros = await inputPaths.enumerated().parallelUnorderedMap { (idx, inputPath) -> [Crasher] in
      defer {
        progress.add(1, ordering: .releasing)
      }
      var results: [Crasher] = []
      var errors: [any Error] = []
      do {
        // Evaluate the batches.
        let batch = try self.getPotentialCrashers(
          for: inputPath, frontendArgs: frontendArgs
        )
        results = await batch.eval { input in
          do {
            guard let crasher = try await self.getCrasher(input) else {
              return nil
            }
            // We initially assume determinism and source order to make the
            // initial gathering quicker, make sure we undo that here.
            return crasher
              .withDeterministic(false)
              .withSourceOrderCompletion(false)
          } catch {
            errors.append(error)
            return nil
          }
        }
      } catch {
        log.warning("\(error)")
        return []
      }
      defer {
        for result in results {
          seenSigs.formUnion(result.signatures.sigs)
        }
      }
      if !self.quickMode, results.isEmpty {
        if let error = errors.last {
          log.warning("\(error)")
        } else {
          log.warning("\(inputPath.fileName) didn't reproduce issue")
          if let noreproDir = self.noreproDir {
            let outPath = noreproDir.appending(inputPath.fileName)
            if outPath.exists && self.deleteInputs {
              inputPath.remove()
            } else {
              try? FileManager.default.moveItem(
                atPath: inputPath.rawPath,
                toPath: outPath.rawPath
              )
            }
          }
        }
        return []
      }
      guard results.contains(where: { result in
        let sigs = result.signatures
        if self.firstNewSignature(sigs) == nil {
          return false
        }
        if self.quickMode, seenSigs.isSuperset(of: sigs.sigs) {
          return false
        }
        return true
      }) else {
        if self.deleteInputs {
          inputPath.remove()
        }
        return []
      }
      return results
    }.flatMap { $0 }.sorted(by: \.path.rawPath)

    log.prefixFn = nil

    var groupedRepros: [Signature: [Crasher]] = [:]
    for crasher in repros {
      let sigs = crasher.signatures
      seenSigs.formUnion(sigs.sigs)
      guard let sig = firstNewSignature(sigs) else {
        continue
      }
      groupedRepros[sig, default: []].append(crasher)
    }

    var processedPaths = Set(repros.map(\.path))
    let worklist = TaskWorklist<[Reproducer]>()
    for reproGroup in groupedRepros.sorted(by: \.key).map(\.value) {
      worklist.addTask {
        // Take the first reproducer in the group that reduces successfully.
        for repro in reproGroup {
          do {
            return try await self.reduce(repro)
          } catch {
            log.warning("\(error)")
          }
        }
        processedPaths.subtract(reproGroup.map(\.path))
        return []
      }
    }
    for await reproGroup in worklist.results {
      for reduced in reproGroup {
        do {
          try self.writeReproducer(reduced)
        } catch {
          log.warning("\(error)")
          processedPaths.remove(reduced.originalPath!)
        }
      }
    }

    if deleteInputs {
      for path in processedPaths {
        path.remove()
      }
    }

    let delta = Int(Date().timeIntervalSince(start).rounded())
    log.info("""
      Finished processing \(inputPaths.count) files in \(delta)s, \
      \(seenSigs.count) unique signatures, \(groupedRepros.count) new signatures
      """
    )

    if fileIssues {
      try await fileReproIssues()
    }
  }

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
            ["--tidy", "--not-c", testPath.rawPath] + inputs.map(\.rawPath)
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

  private func checkCrash(
    of input: PotentialCrasher,
    matchingSignatures signatures: KnownSignatures? = nil
  ) async throws -> Crasher? {
    try await input.withInputFiles { paths in
      guard let crash = try await toolchain.checkCrash(
        of: paths, options: input.options, matchingSignatures: signatures
      ) else {
        return nil
      }
      return Crasher(input: input, crashInfo: .init(crash))
    }
  }

  private func checkDeterministicCrash(
    of input: PotentialCrasher
  ) async throws -> Crasher? {
    try await input.withInputFiles { paths in
      guard let crashInfo = try await toolchain.checkDeterministicCrash(
        of: paths, options: input.options
      ) else {
        return nil
      }
      return Crasher(input: input.withDeterministic(), crashInfo: crashInfo)
    }
  }

  /// The directory to place non-reproducing crashers into. Only used when an
  /// input directory is given and input deletion is enabled.
  private lazy var noreproDir: AbsolutePath? = {
    guard let inputDir, deleteInputs else { return nil }
    let result = inputDir.appending("norepro")
    if !result.exists {
      try? result.makeDir()
    }
    return result
  }()

  private func getCrasher(_ input: PotentialCrasher) async throws -> Crasher? {
    log.info("checking: \(input)...".withColor(.gray))
    guard let crasher = try await checkCrash(of: input) else { return nil }
    log.info("found: \(crasher)")
    return crasher
  }

  /// Given a non-deterministic crasher, try some different options to see if
  /// they can make it deterministic.
  private func makeDeterministicIfNeeded(
    _ crasher: Crasher
  ) async throws -> Crasher {
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

  private enum PotentialCrasherBatch {
    case empty
    case one(PotentialCrasher)
    case allOf([PotentialCrasherBatch])
    case firstOf([PotentialCrasherBatch])
    case lastOf([PotentialCrasherBatch])

    var isEmpty: Bool {
      if case .empty = self { true } else { false }
    }

    /// Make the batch more resilient to non-deterministic failures by
    /// attempting multiple times and trying things like guard malloc.
    var withNonDeterminismHandling: Self {
      .firstOf([
        self,
        self.map(\.withGuardMalloc),
        self.map { $0.withDeterministic(false) },
      ])
    }

    static func base(_ crasher: PotentialCrasher) -> Self {
      if crasher.buffers.count > 1 {
        var result: [PotentialCrasher] = [crasher.withJoinedBuffers()]
        if crasher.options.kind == .complete {
          // Completion requires a primary file.
          result += crasher.buffers.indices.map {
            crasher.withPrimaryIdx($0)
          }
        } else {
          result.append(crasher)
        }
        return .firstOf(result.map(one))
      } else {
        return .one(crasher)
      }
    }

    func map(
      _ fn: (PotentialCrasher) throws -> PotentialCrasher
    ) rethrows -> Self {
      switch self {
      case .empty:
        return self
      case .one(let crasher):
        return try .one(fn(crasher))
      case .allOf(let elts):
        return .allOf(try elts.map { try $0.map(fn) })
      case .firstOf(let elts):
        return .firstOf(try elts.map { try $0.map(fn) })
      case .lastOf(let elts):
        return .lastOf(try elts.map { try $0.map(fn) })
      }
    }

    func compactMap(
      _ fn: (PotentialCrasher) throws -> PotentialCrasher?
    ) rethrows -> Self {
      switch self {
      case .empty:
        return self
      case .one(let crasher):
        guard let crasher = try fn(crasher) else { return .empty }
        return .one(crasher)
      case .allOf(let elts):
        let elts = try elts.compactMap { try $0.compactMap(fn) }
        return elts.isEmpty ? .empty : .allOf(elts)
      case .firstOf(let elts):
        let elts = try elts.compactMap { try $0.compactMap(fn) }
        return elts.isEmpty ? .empty : .firstOf(elts)
      case .lastOf(let elts):
        let elts = try elts.compactMap { try $0.compactMap(fn) }
        return elts.isEmpty ? .empty : .lastOf(elts)
      }
    }

    func eval<T>(@_inheritActorContext _ fn: @Sendable (PotentialCrasher) async throws -> T?) async rethrows -> [T] {
      switch self {
      case .empty:
        return []
      case .one(let crasher):
        guard let result = try await fn(crasher) else { return [] }
        return [result]
      case .allOf(let elts):
        var results: [T] = []
        for elt in elts {
          results += try await elt.eval(fn)
        }
        return results
      case .firstOf(let elts):
        for elt in elts {
          let result = try await elt.eval(fn)
          guard !result.isEmpty else {
            continue
          }
          return result
        }
        return []
      case .lastOf(let elts):
        var currentResult: [T] = []
        for elt in elts {
          let result = try await elt.eval(fn)
          guard !result.isEmpty else { break }
          currentResult = result
        }
        return currentResult
      }
    }
  }

  /// For a given input crasher, produce a batch of potential crasher
  /// configurations to try.
  private func getPotentialCrashers(
    for path: AbsolutePath, frontendArgs: [Command.Argument]
  ) throws -> PotentialCrasherBatch {
    typealias Batch = PotentialCrasherBatch

    let input = try FuzzerInput(from: path)

    // If we have custom frontend args, then only try those.
    let frontendArgs = input.header.frontendArgs ?? frontendArgs
    if !frontendArgs.isEmpty {
      return .one(.custom(input, frontendArgs: frontendArgs))
        .withNonDeterminismHandling
    }

    let completeBatch: Batch = try PotentialCrasher.completion(input).map {
      Batch.base($0.withSourceOrderCompletion(true).withSolverLimits())
    } ?? .empty

    let compileBatch = Batch.firstOf([
      .base(.typecheck(input).withSolverLimits()),
      .lastOf([
        .emitIR(input), .emitSIL(input), .emitSILGen(input)
      ].map(Batch.base))
    ])

    var batch = Batch.allOf([compileBatch, completeBatch])

    if !quickMode {
      // If the initial batch doesn't reproduce, try with no solver limits.
      var extendedBatch = batch.compactMap {
        $0.hasSolverLimits ? $0.withSolverLimits(false) : nil
      }

      // Then try with no SDK
      extendedBatch = .firstOf([
        extendedBatch, batch.map { $0.withNoSDK.withSolverLimits(false) }
      ])

      // Then try with no objc.
      extendedBatch = .firstOf([
        extendedBatch, extendedBatch.map(\.withNoObjCInterop)
      ])

      // If we still don't have a result, try the batches again with
      // non-determinism in mind.
      extendedBatch = extendedBatch.withNonDeterminismHandling
      batch = .firstOf([batch, extendedBatch])
    }

    // Default to Swift 6
    return batch.map { $0.withLanguageMode(6) }
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

  private func reduce(_ crasher: Crasher) async throws -> [Reproducer] {
    var reproducers: [Reproducer] = []
    var crasher = crasher

    // Check to see if we can drop the language mode and solver limits.
    // FIXME: We shouldn't be doing this here
    if crasher.input.options.languageMode != nil {
      let newInput = crasher.input
        .withLanguageMode(nil).withDeterministic(false)
      if let newCrasher = try await checkCrash(of: newInput) {
        crasher = newCrasher
      }
    }
    if crasher.input.options.withSolverLimits {
      let newInput = crasher.input
        .withSolverLimits(false).withDeterministic(false)
      if let newCrasher = try await checkCrash(of: newInput) {
        crasher = newCrasher
      }
    }

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
