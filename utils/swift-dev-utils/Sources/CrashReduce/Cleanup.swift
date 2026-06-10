//===--- Cleanup.swift ----------------------------------------------------===//
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

import SwiftSyntax
import SwiftParser
import SwiftBasicFormat
import SwiftFormat
import SwiftOperators

public enum CleanupKind: String, CaseIterable {
  case basicFormat, fix, swiftFormat
}

final class CleanupFormatter: BasicFormat {
  let kind: CleanupKind

  init(kind: CleanupKind) {
    self.kind = kind
    super.init(
      indentationWidth: .spaces(2),
      viewMode: kind == .fix ? .fixedUp : .sourceAccurate
    )
  }

  static func cleanup(_ tree: some SyntaxProtocol, kind: CleanupKind) -> String {
    Self(kind: kind).rewrite(tree).description
  }

  override func transformTokenPresence(_ token: TokenSyntax) -> SourcePresence? {
    guard kind == .fix, token.presence == .missing else { return nil }
    return .present
  }
}

extension SourceFileSyntax {
  init(text: String) {
    var parser = Parser(text)
    self = SourceFileSyntax.parse(from: &parser)
  }
}

extension Code {
  /// Temporarily replace any completion tokens with editor placeholders for
  /// more friendly swift-syntax handling. On exit, the placeholders will be
  /// turned back into completion tokens.
  func withCompletionTokensReplaced<E: Error>(
    _ body: (String) throws(E) -> String
  ) throws(E) -> Self {
    // First look for any existing placeholders.
    var knownPlaceholders: Set<String> = []
    text.scanningUTF8 { scanner in
      while scanner.hasInput {
        if scanner.tryEat(utf8: "<#") {
          let wordStart = scanner.cursor
          scanner.skip(while: \.isWordChar)
          let wordEnd = scanner.cursor
          if scanner.tryEat(utf8: "#>") {
            knownPlaceholders.insert(scanner.decodeUTF8(wordStart ..< wordEnd))
          }
          continue
        }
        _ = scanner.eat()
      }
    }

    var replacements: [String: String] = [:]
    var replacedText = text.scanningUTF8 { scanner in
      let bytes = scanner.consumeWhole { consumer in
        var tmp = consumer
        guard tmp.trySkip(utf8: "#^") else { return }
        let range = tmp.skip(while: \.isWordChar)
        guard tmp.trySkip(utf8: "^#") else { return }
        consumer = tmp

        let str = range.map { consumer.decodeUTF8($0) } ?? ""
        let replacement = replacements[str] ?? {
          var (replacement, i) = ("CRASH_REDUCE_CC_TOK_", 0)
          while knownPlaceholders.contains(replacement) {
            replacement = "\(str)\(i)"
            i += 1
          }
          let placeholder = "<#\(replacement)#>"
          replacements[str] = placeholder
          return placeholder
        }()
        consumer.append(utf8: replacement)
      }
      return bytes.isUnchanged ? text : String(utf8: bytes)
    }
    replacedText = try body(replacedText)

    // Put the completion tokens back.
    for (ccTok, placeholder) in replacements {
      replacedText = replacedText.replacing(placeholder, with: "#^\(ccTok)^#")
    }
    return Self(replacedText)
  }

  public func cleanupSyntax(_ kind: CleanupKind) throws -> Self {
    // Replace any completion tokens with editor placeholders since the syntax
    // tree doesn't understand them.
    try withCompletionTokensReplaced { text in
      let tree = SourceFileSyntax(text: text)
      switch kind {
      case .basicFormat, .fix:
        return CleanupFormatter.cleanup(tree, kind: kind)
      case .swiftFormat:
        let config = Configuration()
        var output = ""
        let operatorTable = OperatorTable.standardOperators
        try SwiftFormatter(configuration: config).format(
          syntax: operatorTable.foldAll(tree) { _ in }.as(SourceFileSyntax.self)!,
          source: text,
          operatorTable: operatorTable,
          assumingFileURL: nil,
          selection: .infinite,
          to: &output
        )
        return output
      }
    }
  }
}
