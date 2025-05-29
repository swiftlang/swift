//===--- Fingerprint.swift -------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_spi(RawSyntax)
import SwiftSyntax
import SwiftIfConfig
import ASTBridging

struct Fingerprint: Equatable {
  typealias Core = (UInt64, UInt64)
  var core: Core
  init(core: Core) {
    self.core = core
  }

  static func == (lhs: Self, rhs: Self) -> Bool {
    return lhs.core == rhs.core
  }
}

extension StableHasher {
  @inline(__always)
  mutating func combine(text: SyntaxText) {
    let buffer = UnsafeRawBufferPointer(
      start: UnsafeRawPointer(text.baseAddress),
      count: text.count
    )
    self.combine(UInt(truncatingIfNeeded: buffer.count))
    self.combine(bytes: buffer)
  }
}

final class FingerprintVisitor: SyntaxVisitor {
  let configuredRegions: ConfiguredRegions
  var hasher: StableHasher

  init(configuredRegions: ConfiguredRegions) {
    self.configuredRegions = configuredRegions
    self.hasher = StableHasher()
    super.init(viewMode: .sourceAccurate)
  }

  func finalize() -> Fingerprint {
    Fingerprint(core: hasher.finalize())
  }

  override func visit(_ token: TokenSyntax) -> SyntaxVisitorContinueKind {
    // Collect all token texts.
    hasher.combine(text: token.rawText)
    return .skipChildren
  }

  override func visit(_ node: MemberBlockSyntax) -> SyntaxVisitorContinueKind {
    // Skip nominal decl / extension member blocks.
    return .skipChildren
  }

  override func visit(_ node: CodeBlockSyntax) -> SyntaxVisitorContinueKind {
    // Skip function bodies.
    // FIXME: Don't skip closure bodies or other control flow statement blocks.
    //        E.g. 'var val = { if condition { "foo" } else { "bar" } }()'
    return .skipChildren
  }

  override func visit(_ node: IfConfigDeclSyntax) -> SyntaxVisitorContinueKind {
    if let active = configuredRegions.activeClause(for: node) {
      self.walk(active)
    }
    return .skipChildren
  }
}

@_cdecl("swift_ASTGen_getSourceFileFingerprint")
func getSourceFileFingerprint(
  sourceFilePtr: UnsafeMutableRawPointer,
  ctx: BridgedASTContext
) -> BridgedFingerprint {
  let sourceFile = sourceFilePtr.assumingMemoryBound(to: ExportedSourceFile.self)
  let configuredRegions = sourceFile.pointee.configuredRegions(astContext: ctx)
  let visitor = FingerprintVisitor(configuredRegions: configuredRegions)
  visitor.walk(sourceFile.pointee.syntax)
  return visitor.finalize().bridged
}

extension ASTGenVisitor {
  func generateFingerprint(declGroup node: some DeclGroupSyntax) -> Fingerprint {
    let visitor = FingerprintVisitor(configuredRegions: self.configuredRegions)
    visitor.walk(node.memberBlock)
    return visitor.finalize()
  }
}
