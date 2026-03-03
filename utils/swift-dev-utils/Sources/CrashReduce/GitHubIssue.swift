//===--- GitHubIssue.swift ------------------------------------------------===//
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
import RegexBuilder

struct GitHubIssue: Codable {
  var title: String
  var body: String
  var labels: [Label]

  private static let nameLookupRegex =
    /namelookup|ast_scope|ASTScope/.ignoresCase()

  private static let semaRegex =
    /typecheck|constraint|solution|ExprRewriter|verifyChecked|checker/.ignoresCase()

  private static let genericsRegex =
    /requirementmachine|generic(signature|environment)/.ignoresCase()

  private static let protocolsRegex =
    /conformance|witness|associatedtype/.ignoresCase()

  private static func getComponentLabel(for sig: String) -> Set<Label>? {
    if sig.firstMatch(of: /parse/.ignoresCase()) != nil {
      return [.parser]
    }
    if sig.firstMatch(of: nameLookupRegex) != nil {
      return [.nameLookup]
    }
    if sig.firstMatch(of: genericsRegex) != nil {
      return [.typeChecker, .generics]
    }
    if sig.firstMatch(of: protocolsRegex) != nil {
      return [.typeChecker, .protocol]
    }
    if sig.firstMatch(of: semaRegex) != nil {
      return [.typeChecker]
    }
    return nil
  }

  init?(from repro: Reproducer, crashLog: CrashLog) {
    let sig = repro.primarySig
    guard let shortSig = sig.short else {
      log.warning("failed to get short signature for \(sig)")
      return nil
    }
    let joiner = if sig.isAssertion { "with" } else { "in" }
    let crashAdv = repro.options.isDeterministic ? "" : " non-deterministically"
    self.title = "Fuzzed code\(crashAdv) crashes \(joiner) `\(shortSig)`"
    self.body = """
      The following test case\(crashAdv) crashes the compiler:

      ```swift
      \(repro.allBuffers)
      ```

      Stack dump:

      ```
      \(crashLog.signature.assertion.map { $0.fullMessage + "\n" } ?? "")\
      \(crashLog.frames.map(\.line).joined(separator: "\n"))
      ```
      """
    var labels: Set<Label> = [.bug, .foundByFuzzer, .compiler, .crash]
    if let componentLabels = Self.getComponentLabel(for: shortSig.symbol) {
      labels.formUnion(componentLabels)
    } else {
      labels.insert(.triageNeeded)
    }
    self.labels = labels.sorted(by: \.rawValue)
  }

  func post(owner: String, repoName: String) async throws -> Response {
    guard let token = ProcessInfo.processInfo.environment["GITHUB_AUTH_TOKEN"] else {
      struct AuthTokenError: Error {}
      throw AuthTokenError()
    }

    let response = try await run(
      .name("curl"), arguments: [
        "-L",
        "-X", "POST",
        "-H", "Authorization: Bearer \(token)",
        "-H", "X-GitHub-Api-Version: 2022-11-28",
        "https://api.github.com/repos/\(owner.urlPathEscaped)/\(repoName.urlPathEscaped)/issues",
        "-d", String(decoding: try JSONEncoder().encode(self), as: UTF8.self),
      ],
      output: .bytes(limit: .max),
    ).standardOutput
    do {
      return try JSONDecoder().decode(Response.self, from: Data(response))
    } catch {
      log.warning("failed to post issue: \(String(decoding: response, as: UTF8.self))")
      throw error
    }
  }
}

extension GitHubIssue {
  enum Label: String, Codable {
    case bug
    case crash
    case compiler
    case parser
    case generics
    case `protocol`
    case conformances
    case nameLookup = "name lookup"
    case typeChecker = "type checker"
    case triageNeeded = "triage needed"
    case foundByFuzzer = "found by fuzzer"
  }
}

extension GitHubIssue {
  struct Response: Codable {
    enum CodingKeys: String, CodingKey {
      case htmlURL = "html_url"
      case number
    }
    var htmlURL: String
    var number: Int
  }
}

extension String {
  var urlPathEscaped: String {
    addingPercentEncoding(withAllowedCharacters: .urlPathAllowed)!
  }
}
