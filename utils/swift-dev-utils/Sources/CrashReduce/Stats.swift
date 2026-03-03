//===--- Stats.swift ------------------------------------------------------===//
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

@_spi(RawSyntax) public import SwiftSyntax
import SwiftParser
import Foundation

public final class SyntaxStatCollector : SyntaxVisitor {
  var frequencies: [RawTokenKind: Int] = [:]
  var keywordFrequencies: [Keyword: Int] = [:]

  override public func visit(_ node: TokenSyntax) -> SyntaxVisitorContinueKind {
    frequencies[node.rawTokenKind, default: 0] += 1
    if case .keyword(let kwKind) = node.tokenKind {
      keywordFrequencies[kwKind, default: 0] += 1
    }
    return .visitChildren
  }

  public static func collect(in path: AbsolutePath) throws {
    let collector = Self(viewMode: .sourceAccurate)
    for file in try FileManager.default.subpathsOfDirectory(atPath: path.rawPath) where file.hasExtension(.swift) {
      let absPath = path.appending(RelativePath(file))
      let contents = String(decoding: try absPath.read(), as: UTF8.self)
      collector.walk(SourceFileSyntax(text: contents))
    }
    print("freqencies:")
    do {
      let totalFreq = Double(collector.frequencies.reduce(0, { $0 + $1.value }))
      for (kind, count) in collector.frequencies.sorted(by: \.value) {
        print("\(kind) -> \(String(format: "%.2f", 1000 * Double(count) / totalFreq))")
      }
    }
    print("kw freqencies:")
    do {
      let totalFreq = Double(collector.keywordFrequencies.reduce(0, { $0 + $1.value }))
      for (kind, count) in collector.keywordFrequencies.sorted(by: \.value) {
        print("\(kind) -> \(String(format: "%.2f", 1000 * Double(count) / totalFreq))")
      }
    }
  }
}
