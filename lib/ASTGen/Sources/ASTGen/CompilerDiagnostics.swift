//===--- CompilerDiagnostics.swift ----------------------------------------===//
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
//
// Support for emitting the compiler's own diagnostics -- the ones declared in
// the `Diagnostics*.def` files and emitted from C++ as `diag::some_diagnostic`
// -- from ASTGen.
//
// Prefer these to `ASTGenDiagnostic`, which describes a diagnostic with an
// ad-hoc string. A diagnostic declared in a `.def` file participates in
// diagnostic groups, education notes, and `-Wwarning`/`@diagnose` group
// behavior control, and it has exactly one definition shared with the C++
// parser rather than a copy that can drift from it. `ASTGenDiagnostic` remains
// appropriate for reporting internal ASTGen inconsistencies, which have no
// counterpart in the C++ compiler.
//
// The per-diagnostic entry points are generated into `DiagnosticsParse.swift`
// from `include/swift/AST/DiagnosticsParse.def`; see
// `utils/generate_swift_diagnostics.py`.
//
//===----------------------------------------------------------------------===//

import ASTBridging
import BasicBridging
@_spi(RawSyntax) import SwiftSyntax

/// An argument to a compiler diagnostic.
///
/// Arguments are held in their Swift form until the diagnostic is emitted,
/// because a `DiagnosticArgument` holding a string does not own its storage:
/// the string has to outlive the diagnostic, which may be queued rather than
/// emitted immediately.
enum CompilerDiagnosticArgument {
  case string(String)
  case identifier(Identifier)
  case integer(Int)
  case unsigned(UInt)
  case boolean(Bool)

  /// Bridge this argument, allocating any storage it needs in `ctx`.
  fileprivate func bridged(in ctx: BridgedASTContext) -> BridgedDiagnosticArgument {
    switch self {
    case .string(let value):
      return BridgedDiagnosticArgument(ctx.allocateCopy(value))
    case .identifier(let value):
      return BridgedDiagnosticArgument(value)
    case .integer(let value):
      return BridgedDiagnosticArgument(value)
    case .unsigned(let value):
      return .unsigned(UInt32(truncatingIfNeeded: value))
    case .boolean(let value):
      // The diagnostic engine has no dedicated boolean argument kind; a
      // diagnostic declared as taking `bool` is formatted through the integer
      // path, which is what `%select` expects.
      return BridgedDiagnosticArgument(value ? 1 : 0)
    }
  }
}

/// A diagnostic declared in one of the compiler's `Diagnostics*.def` files,
/// together with the arguments it was declared to take.
///
/// Construct one using the generated static members, whose names match the
/// diagnostic IDs used from C++:
///
///     self.diagnose(.expected_expr, at: node)
///     self.diagnose(.attr_expected_rparen(attrName, false), at: node.rightParen)
struct CompilerDiagnostic {
  let id: swift.DiagID
  let arguments: [CompilerDiagnosticArgument]

  init(_ id: swift.DiagID, _ arguments: [CompilerDiagnosticArgument] = []) {
    self.id = id
    self.arguments = arguments
  }
}

/// A fix-it attached to a compiler diagnostic, replacing the source text of
/// `range` with `replacement`.
struct CompilerDiagnosticFixIt {
  let range: CharSourceRange
  let replacement: String

  init(replace range: CharSourceRange, with replacement: String) {
    self.range = range
    self.replacement = replacement
  }
}

extension ASTGenVisitor {
  /// Emit `diagnostic` at the start of `node`, excluding leading trivia.
  func diagnose(
    _ diagnostic: CompilerDiagnostic,
    at node: some SyntaxProtocol,
    highlight: Syntax? = nil,
    fixIts: [CompilerDiagnosticFixIt] = []
  ) {
    self.diagnose(
      diagnostic,
      at: self.generateSourceLoc(node),
      highlight: highlight,
      fixIts: fixIts
    )
  }

  /// Emit `diagnostic` at `loc`.
  ///
  /// A nil `loc` still emits the diagnostic, without a location, matching how
  /// the C++ parser behaves when it has no location to point at.
  func diagnose(
    _ diagnostic: CompilerDiagnostic,
    at loc: SourceLoc,
    highlight: Syntax? = nil,
    fixIts: [CompilerDiagnosticFixIt] = []
  ) {
    let highlightRange = highlight.map { self.generateCharSourceRange($0) }

    let bridgedArguments = diagnostic.arguments.map { $0.bridged(in: self.ctx) }
    let bridgedFixIts = fixIts.map { fixIt in
      BridgedDiagnosticFixIt(
        fixIt.range.getStart(),
        fixIt.range.getByteLength(),
        self.ctx.allocateCopy(fixIt.replacement)
      )
    }

    // The diagnostic engine copies the argument and fix-it arrays, so they only
    // need to stay alive across the call itself. The storage they *point* to
    // has already been allocated in the ASTContext above.
    bridgedArguments.withUnsafeBufferPointer { arguments in
      bridgedFixIts.withUnsafeBufferPointer { fixIts in
        self.diagnosticEngine.diagnose(
          at: loc,
          diagnostic.id,
          BridgedArrayRef(data: arguments.baseAddress, count: arguments.count),
          highlightAt: highlightRange?.getStart() ?? nil,
          highlightLength: highlightRange?.getByteLength() ?? 0,
          fixIts: BridgedArrayRef(data: fixIts.baseAddress, count: fixIts.count)
        )
      }
    }
  }

  /// The character range covering `node`, excluding trivia.
  func generateCharSourceRange(_ node: some SyntaxProtocol) -> CharSourceRange {
    self.generateCharSourceRange(
      start: node.positionAfterSkippingLeadingTrivia,
      length: node.trimmedLength
    )
  }
}

extension BridgedASTContext {
  /// Allocate an `ASTContext`-owned copy of `string`.
  ///
  /// Needed because a `DiagnosticArgument` holding a string borrows it, and a
  /// Swift `String`'s storage does not outlive the expression it appears in.
  func allocateCopy(_ string: String) -> BridgedStringRef {
    var string = string
    return string.withUTF8 { utf8 in
      self.allocateCopy(
        string: BridgedStringRef(data: utf8.baseAddress, count: utf8.count)
      )
    }
  }
}
