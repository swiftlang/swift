//===--- Reproducer.swift -------------------------------------------------===//
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

import CryptoKit
import Foundation
import System
import Subprocess
import Synchronization

/// A crasher that has been reproduced.
public struct Reproducer: Sendable {
  var signatures: KnownSignatures
  var options: Options
  var buffers: [Buffer]

  var originalPath: AbsolutePath?
  var originalID: String?
  var crashInfo: CrashInfo?
  var isStackOverflow: Bool
  var issueID: Int?

  var kind: Options.Kind { options.kind }

  var primarySig: Signature {
    signatures.primary
  }

  init(_ crasher: Crasher) {
    let input = crasher.input
    let crash = crasher.crashInfo
    self.init(
      signatures: crash.signatures, options: input.options, buffers: input.buffers,
      originalID: input.id, originalPath: crasher.path, crashInfo: crash,
      isStackOverflow: crash.primary.isStackOverflow, issueID: nil
    )
  }

  init(
    signatures: KnownSignatures, options: Options, buffers: [Buffer],
    originalID: String?, originalPath: AbsolutePath?, crashInfo: CrashInfo?,
    isStackOverflow: Bool, issueID: Int?
  ) {
    self.signatures = signatures
    self.options = options
    self.buffers = buffers
    self.crashInfo = crashInfo
    self.isStackOverflow = isStackOverflow
    self.issueID = issueID
    self.originalID = originalID
    self.originalPath = originalPath
  }

  static func decoding(_ data: Data) throws -> Self? {
    let code = try Code(decoding: data)
    guard let header = try code.reproHeader else { return nil }
    let buffers = try Buffer.makeDefault(
      code.cleanupTrivia(includingWhitespace: false)
        .split(header.splits ?? []).map {
          // fixme...
          $0.cleanupTrivia(includingWhitespace: false)
        }
    )
    var signatures = KnownSignatures(
      Signature(symbol: header.signature, assertion: header.signatureAssert)
    )
    for alias in header.aliases ?? [] {
      // FIXME: Dropping asserts...
      if let assert = Assertion(from: alias) {
        signatures.add(Signature(symbol: nil, assertion: assert))
      } else {
        signatures.add(Signature(symbol: alias, assertion: nil))
      }
    }
    return Self(
      signatures: signatures,
      options: .init(
        kind: header.kind ?? .typecheck,
        isDeterministic: header.isDeterministic ?? true,
        useGuardMalloc: header.useGuardMalloc ?? false,
        useSourceOrderCompletion: header.useSourceOrderCompletion ?? false,
        withSolverLimits: header.solverLimits ?? false,
        noSDK: header.noSDK ?? false,
        noObjCInterop: header.noObjCInterop ?? false,
        languageMode: header.languageMode,
        diagnosticStyle: header.diagnosticStyle,
        frontendArgs: header.frontendArgs?.map { .value($0) } ?? []
      ),
      buffers: buffers,
      originalID: header.original,
      originalPath: nil,
      crashInfo: nil,
      isStackOverflow: header.stackOverflow ?? false,
      issueID: header.issueID
    )
  }

  private func getTestCaseRequirements() -> String {
    enum TestCaseRequirement: String, Hashable {
      case macOS = "OS=macosx"
      case targetSameAsHost = "target-same-as-host"
      case noAsan = "no_asan"
      case objcInterop = "objc_interop"
      case swiftParser = "swift_swift_parser"
    }
    var requirements: Set<TestCaseRequirement> = []
    if options.useGuardMalloc {
      requirements.formUnion([.macOS, .targetSameAsHost, .noAsan])
    }
    if buffers.contains(where: \.code.hasImport) {
      requirements.insert(.macOS)
    }
    if buffers.contains(where: \.code.hasObjC) && !options.noObjCInterop {
      requirements.insert(.objcInterop)
    }
    if signatures.sigs.contains(where: { $0.symbol?.hasPrefix("$s") ?? false }) {
      requirements.insert(.swiftParser)
    }
    var result = requirements.map(\.rawValue).sorted().map {
      "// REQUIRES: \($0)"
    }
    if !options.isDeterministic {
      // This isn't a real lit feature, this effectively just disables the test
      // until it can be investigated.
      result.append("// REQUIRES: non_deterministic_crasher")
    }
    if result.isEmpty {
      return ""
    }
    return "\n" + result.joined(separator: "\n")
  }

  private func getTestCaseInvocation(_ inputs: [String]) -> String {
    // FIXME: We ought to factor out common logic with `getCommandInvocation`.
    var extraOpts: [String] = []
    if options.noSDK {
      extraOpts.append("-sdk %t")
    }
    if options.noObjCInterop {
      extraOpts.append("-disable-objc-interop")
    }
    if options.withSolverLimits {
      extraOpts.append(options.solverLimitArgs.joined(separator: " "))
    }
    if let languageMode = options.languageMode {
      extraOpts.append("-swift-version \(languageMode)")
    }
    if let diagnosticStyle = options.diagnosticStyle {
      extraOpts.append("-diagnostic-style=\(diagnosticStyle)")
    }
    let extraOptsStr = (extraOpts.isEmpty ? "" : " ") + extraOpts.joined(separator: " ")
    let sourceOrderCompletion = options.useSourceOrderCompletion ? " -source-order-completion" : ""

    var inv = switch kind {
    case .typecheck:
      "%target-swift-frontend -typecheck\(extraOptsStr)"
    case .emitSILGen:
      "%target-swift-frontend -emit-silgen\(extraOptsStr)"
    case .emitSIL:
      "%target-swift-frontend -emit-sil\(extraOptsStr)"
    case .emitIR:
      "%target-swift-frontend -emit-ir\(extraOptsStr)"
    case .complete:
      "%target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics\(extraOptsStr)\(sourceOrderCompletion) -source-filename"
    case .custom:
      "%target-swift-frontend \(options.frontendArgs.map(\.printed).joined(separator: " "))"
    }
    inv += " "
    inv += options.reorderInputs(inputs).joined(separator: " ")
    return inv
  }

  var allBuffers: String {
    if buffers.count > 1 {
      buffers.map { buffer in
        """
        //--- \(buffer.name)
        \(buffer.code.text)
        """
      }.joined(separator: "\n")
    } else {
      buffers[0].code.text
    }
  }

  var splits: [Int] {
    var result: [Int] = []
    for buffer in buffers.dropLast() {
      result.append(buffer.code.text.utf8.count + (result.last ?? 0))
    }
    return result
  }

  func serialize() throws -> Data {
    let splits = self.splits
    let aliases = signatures.secondaries.sorted()
    let header = Header(
      kind: options.kind,
      isDeterministic: options.isDeterministic ? nil : false,
      signature: signatures.primary.description,
      signatureAssert: signatures.primary.assertion,
      stackOverflow: isStackOverflow ? true : nil,
      aliases: aliases.isEmpty ? nil : aliases.map(\.description),
      useGuardMalloc: options.useGuardMalloc ? true : nil,
      useSourceOrderCompletion: options.useSourceOrderCompletion ? true : nil,
      solverLimits: options.withSolverLimits ? true : nil,
      noSDK: options.noSDK ? true : nil,
      noObjCInterop: options.noObjCInterop ? true : nil,
      languageMode: options.languageMode,
      diagnosticStyle: options.diagnosticStyle,
      issueID: issueID,
      original: originalID,
      splits: splits.isEmpty ? nil : splits,
      frontendArgs: options.frontendArgs.isEmpty ? nil
                      : options.frontendArgs.flatMap(\.rawArgs)
    )
    let encoder = JSONEncoder()
    encoder.outputFormatting = [.sortedKeys, .withoutEscapingSlashes]
    let headerJSON = String(
      decoding: try encoder.encode(header), as: UTF8.self
    )
    let multiBuffer = buffers.count > 1
    let needsEmptyDir = options.noSDK || multiBuffer
    let emptyDir = needsEmptyDir ? "// RUN: %empty-directory(%t)\n" : ""
    let splitFile = multiBuffer ? "// RUN: split-file %s %t\n" : ""
    let env = options.useGuardMalloc ? " env DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib" : ""
    let inputs = if multiBuffer { buffers.map { "%t/\($0.name)" } } else { ["%s"] }
    let reqs = getTestCaseRequirements()
    let inv = getTestCaseInvocation(inputs)
    let issueURL = if let issueID {
      "\n// https://github.com/swiftlang/swift/issues/\(issueID)"
    } else {
      ""
    }
    let run = "// RUN:\(env) not --crash \(inv)\(reqs)"
    let contents = """
      // \(headerJSON)
      \(emptyDir)\(splitFile)\(run)\(issueURL)
      \(allBuffers)

      """
    return Data(contents.utf8)
  }
}

extension Reproducer {
  struct Options: Codable, Hashable {
    enum Kind: String, Codable, Hashable {
      case typecheck
      case emitSILGen = "emit-silgen"
      case emitSIL = "emit-sil"
      case emitIR = "emit-ir"
      case complete
      case custom
    }
    var kind: Kind
    var isDeterministic: Bool = true
    var useGuardMalloc: Bool = false
    var useSourceOrderCompletion: Bool = false
    var withSolverLimits: Bool = false
    var noSDK: Bool = false
    var noObjCInterop: Bool = false
    var languageMode: Int?
    var isJoined: Bool = false
    var diagnosticStyle: String?
    var primaryIdx: Int?
    var frontendArgs: [Command.Argument] = []

    static var typecheck: Self { Self(kind: .typecheck) }
    static var emitSILGen: Self { Self(kind: .emitSILGen) }
    static var emitSIL: Self { Self(kind: .emitSIL) }
    static var emitIR: Self { Self(kind: .emitIR) }
    static var complete: Self { Self(kind: .complete) }

    static func custom(frontendArgs: [Command.Argument]) -> Self {
      Self(kind: .custom, frontendArgs: frontendArgs)
    }
  }
  struct Header: Codable {
    var kind: Options.Kind?
    var isDeterministic: Bool?
    var signature: String
    var signatureAssert: Assertion?
    var stackOverflow: Bool?
    var aliases: [String]?
    var useGuardMalloc: Bool?
    var useSourceOrderCompletion: Bool?
    var solverLimits: Bool?
    var noSDK: Bool?
    var noObjCInterop: Bool?
    var languageMode: Int?
    var diagnosticStyle: String?
    var issueID: Int?
    var original: String?
    var splits: [Int]?
    var frontendArgs: [String]?
  }
}

extension Reproducer.Options {
  func reorderInputs(_ inputs: [String]) -> [String] {
    var result: [String] = []
    if let primaryIdx {
      result.append(inputs[primaryIdx])
    }
    for (idx, input) in inputs.enumerated() where idx != primaryIdx {
      result.append(input)
    }
    return result
  }

  func executablePath(for toolchain: Toolchain) -> AbsolutePath {
    switch kind {
    case .typecheck, .emitSILGen, .emitSIL, .emitIR, .custom:
      toolchain.swiftPath
    case .complete:
      toolchain.swiftIDETestPath
    }
  }

  fileprivate var solverLimitArgs: [String] {
    // FIXME: This is really something that should be included in the fuzzer
    // JSON blob.
    [
      "-solver-trail-threshold=100000",
      "-solver-scope-threshold=100000",
      "-solver-memory-threshold=10000000",
    ]
  }

  func getCommandInvocation(
    for inputs: [AbsolutePath], with toolchain: Toolchain
  ) -> Subprocess.Configuration {
    var env: [Environment.Key: String] = [:]
    if useGuardMalloc {
      env["DYLD_INSERT_LIBRARIES"] = "/usr/lib/libgmalloc.dylib"
    }
    let exec = executablePath(for: toolchain)
    var args: [String] = []
    switch kind {
    case .typecheck:
      args += ["-frontend", "-typecheck"]
    case .emitSILGen:
      args += ["-frontend", "-emit-silgen"]
    case .emitSIL:
      args += ["-frontend", "-emit-sil"]
    case .emitIR:
      args += ["-frontend", "-emit-ir"]
    case .complete:
      args += [
        "--code-completion", "-batch-code-completion", "-skip-filecheck",
        "-code-completion-diagnostics", "-source-filename"
      ]
    case .custom:
      args.append("-frontend")
      args += frontendArgs.flatMap(\.rawArgs)
    }
    args += reorderInputs(inputs.map(\.rawPath))
    if useSourceOrderCompletion {
      args.append("-source-order-completion")
    }
    if withSolverLimits {
      args += solverLimitArgs
    }
    if !noSDK {
      args += ["-sdk", toolchain.sdkPath.rawPath]
    }
    if noObjCInterop {
      args.append("-disable-objc-interop")
    }
    if let languageMode {
      args += ["-swift-version", "\(languageMode)"]
    }
    if let diagnosticStyle {
      args.append("-diagnostic-style=\(diagnosticStyle)")
    }
    return Subprocess.Configuration(
      .path(exec.storage), arguments: .init(args),
      environment: .custom(env)
    )
  }
}

extension Code {
  // Find the JSON header in leading comments.
  fileprivate var reproHeader: Reproducer.Header? {
    get throws {
      var text = text
      return try text.withUTF8 { bytes in
        var scanner = ByteScanner(bytes)
        func takeCommentLine() -> UnsafeRawBufferPointer? {
          scanner.skip(while: \.isNewline)
          guard scanner.tryEat(utf8: "//") else { return nil }
          scanner.skip(while: \.isSpaceOrTab)
          return scanner.eat(while: { !$0.isNewline }) ??
            .init(start: nil, count: 0)
        }
        while let headerBytes = takeCommentLine() {
          if headerBytes.first == UInt8(ascii: "{") {
            return try JSONDecoder().decode(
              Reproducer.Header.self, from: Data(headerBytes)
            )
          }
        }
        return nil
      }
    }
  }
}

/// A wrapper around Reproducer for a specific reproducer test case on disk.
final class ReproducerFile {
  var path: AbsolutePath {
    didSet {
      guard path != oldValue else { return }
      do {
        if oldValue.exists {
          if path.exists {
            try FileManager.default.removeItem(atPath: path.rawPath)
          }
          try FileManager.default.moveItem(
            at: URL(filePath: oldValue.storage)!,
            to: URL(filePath: path.storage)!
          )
        }
      } catch {
        log.error("\(error)")
      }
    }
  }
  var reproducer: Reproducer

  private static func computeFileHashPrefix(
    from repro: Reproducer, length: Int
  ) -> String {
    var data = Data()
    for buffer in repro.buffers {
      data.append(Data(buffer.code.text.utf8))
      data.append(UInt8(ascii: "\n"))
    }
    let hash = SHA256.hash(data: data)
    var str = ""
    hash.withUnsafeBytes { bytes in
      for byte in bytes.prefix(length) {
        str += String(format: "%02hhx", byte)
      }
    }
    return str
  }

  private static func symbolNameForFile(for repro: Reproducer) -> String? {
    guard let sym = repro.primarySig.short?.symbol,
          sym.allSatisfy({ $0.isLetter || $0.isNumber || $0 == ":" }) else {
      return nil
    }
    let components = sym.split(separator: "::")
    return components.suffix(2).joined(separator: "-")
  }

  private static func computeDefaultFileName(from repro: Reproducer) -> String {
    let prefix = if let prefix = Self.symbolNameForFile(for: repro) {
      "\(prefix)-"
    } else {
      ""
    }
    let suffix = computeFileHashPrefix(
      from: repro, length: prefix.isEmpty ? 8 : 3
    )
    return "\(prefix)\(suffix).swift"
  }

  init(in directory: AbsolutePath, reproducer: Reproducer) {
    self.path = directory.appending(Self.computeDefaultFileName(from: reproducer))
    self.reproducer = reproducer
  }

  init(path: AbsolutePath, reproducer: Reproducer) {
    self.path = path
    self.reproducer = reproducer
  }

  init?(from path: AbsolutePath) throws {
    guard let reproducer = try Reproducer.decoding(path.read()) else {
      return nil
    }
    self.path = path
    self.reproducer = reproducer
    self.checkFilename(warn: true)
  }

  func checkFilename(warn: Bool) {
    guard !path.fileName.contains("issue-"),
          case let defaultFileName = Self.computeDefaultFileName(from: reproducer),
          defaultFileName != path.fileName else { return }
    if warn {
      log.warning("""
          '\(path.fileName)' does not have expected name, \
          expected '\(defaultFileName)'
          """)
    }
    path = path.parentDir!.appending(defaultFileName)
    try? write()
  }

  func write() throws {
    log.info("writing \(path)")
    try path.write(reproducer.serialize())
  }
}

extension ReproducerFile: CustomStringConvertible {
  var description: String {
    "\(reproducer.signatures) | \(path.fileName)"
  }
}

struct ReproducerError: Error, CustomStringConvertible {
  var message: String
  init(_ message: String) {
    self.message = message
  }
  var description: String {
    message
  }
}

extension Reproducer.Options: CustomStringConvertible {
  var description: String {
    var components = [kind.rawValue]
    if !isDeterministic {
      components.append("non-determ")
    }
    if useGuardMalloc {
      components.append("guard-malloc")
    }
    if useSourceOrderCompletion {
      components.append("source-order")
    }
    if noSDK {
      components.append("no-sdk")
    }
    if noObjCInterop {
      components.append("no-objc")
    }
    if isJoined {
      components.append("joined")
    }
    if withSolverLimits {
      components.append("solver-limits")
    }
    if let languageMode {
      components.append("lang-mode=\(languageMode)")
    }
    if let primaryIdx {
      components.append("primary=\(primaryIdx)")
    }
    return components.joined(separator: ", ")
  }
}
