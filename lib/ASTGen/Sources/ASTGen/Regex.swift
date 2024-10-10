//===--- Regex.swift - SourceLoc bridiging utilities ------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import ASTBridging
import BasicBridging
import SwiftDiagnostics
import SwiftSyntax

#if canImport(_CompilerRegexParser)
@_spi(CompilerInterface) import _CompilerRegexParser

extension ASTGenVisitor {
  func generate(regexLiteralExpr node: RegexLiteralExprSyntax) -> BridgedExpr {
    let str = node.trimmedDescription
    let captureBuffer = BridgedRegexLiteralExpr
      .allocateCaptureStructureSerializationBuffer(self.ctx, size: str.utf8.count)
    let captureBufferOut = UnsafeMutableRawBufferPointer(
      start: UnsafeMutableRawPointer(mutating: captureBuffer.baseAddress),
      count: captureBuffer.count
    )

    let loc = self.generateSourceLoc(node);

    do {
      // FIXME: We need to plumb through the 'regexToEmit' result to the caller.
      // For now, it is the same as the input.
      var regexToEmit: String
      let version: Int
      (regexToEmit, version) = try swiftCompilerParseRegexLiteral(
        str,
        captureBufferOut: captureBufferOut
      )
      // Copy the regex string to the ASTContext.
      let regexToEmitStr = regexToEmit.withBridgedString {
        self.ctx.allocateCopy(string: $0)
      }

      return BridgedRegexLiteralExpr.createParsed(
        self.ctx,
        loc: loc,
        regexText: regexToEmitStr,
        version: version,
        captureStructure: captureBuffer
      ).asExpr
    } catch let error as _CompilerRegexParser.CompilerParseError {
      let offset = error.location != nil ? str.utf8.offset(of: error.location!) : 0
      let position = node.positionAfterSkippingLeadingTrivia.advanced(by: offset)
      self.diagnose(
        Diagnostic(
          node: node.regex,
          position: position,
          message: RegexParserError(error.message)
        )
      )
      return BridgedErrorExpr.create(self.ctx, loc: BridgedSourceRange(start: loc, end: loc)).asExpr
    } catch {
      fatalError("Expected CompilerParseError")
    }
  }
}

/// Bridging between C++ lexer and swiftCompilerLexRegexLiteral.
///
/// Attempt to lex a regex literal string.
///
/// - Parameters:
///   - curPtrPtr: A pointer to the current pointer of lexer, which should be
///                the start of the literal. This will be advanced to the point
///                at which the lexer should resume, or will remain the same if
///                this is not a regex literal.
///   - bufferEndPtr: A pointer to the end of the buffer, which should not be
///                   lexed past.
///   - mustBeRegex: A bool value whether an error during lexing should be
///                  considered a regex literal, or some thing else. If true
///                  advace the curPtrPtr and emit the diagnostic. If false,
///                  curPtrPtr won't be modified.
///   - bridgedDiagnosticEngine: Diagnostic engine to emit diagnostics.
///
/// - Returns: A bool indicating whether lexing was completely erroneous, and
///            cannot be recovered from, or false if there either was no error,
///            or there was a recoverable error.
@_cdecl("swift_ASTGen_lexRegexLiteral")
public func _RegexLiteralLexingFn(
  _ curPtrPtr: UnsafeMutablePointer<UnsafePointer<CChar>>,
  _ bufferEndPtr: UnsafePointer<CChar>,
  _ mustBeRegex: Bool,
  _ bridgedDiagnosticEngine: BridgedNullableDiagnosticEngine
) -> /*CompletelyErroneous*/ Bool {
  let inputPtr = curPtrPtr.pointee

  guard
    let (resumePtr, error) = swiftCompilerLexRegexLiteral(
      start: inputPtr,
      bufferEnd: bufferEndPtr,
      mustBeRegex: mustBeRegex
    )
  else {
    // Not a regex literal, fallback without advancing the pointer.
    return false
  }

  // Advance the current pointer.
  curPtrPtr.pointee = resumePtr.assumingMemoryBound(to: CChar.self)

  if let error = error {
    // Emit diagnostic if diagnostics are enabled.
    if let diagEnginePtr = bridgedDiagnosticEngine.raw {
      var message = error.message
      message.withBridgedString { message in
        BridgedDiagnostic(
          at: BridgedSourceLoc(raw: error.location),
          message: message,
          severity: .error,
          engine: BridgedDiagnosticEngine(raw: diagEnginePtr)
        ).finish()
      }
    }
    return error.completelyErroneous
  }
  return false
}

/// Bridging between C++ parser and swiftCompilerParseRegexLiteral.
///
/// - Parameters:
///   - input: Regex literal text.
///   - versionOut: A buffer accepting a regex literal format version.
///   - captureStructureOut: A buffer accepting a byte sequence representing the
///     capture structure.
///   - captureStructureSize: The size of the capture structure buffer. Must be
///     greater than or equal to `input.size()`.
///   - bridgedDiagnosticBaseLoc: Source location of the start of the literal
///   - bridgedDiagnosticEngine: Diagnostic engine to emit diagnostics.
///
/// - Returns: `true` if there was a parse error, `false` otherwise.
@_cdecl("swift_ASTGen_parseRegexLiteral")
public func _RegexLiteralParsingFn(
  _ input: BridgedStringRef,
  _ versionOut: UnsafeMutablePointer<UInt>,
  _ captureStructureOut: UnsafeMutableRawPointer,
  _ captureStructureSize: UInt,
  _ bridgedDiagnosticBaseLoc: BridgedSourceLoc,
  _ bridgedDiagnosticEngine: BridgedDiagnosticEngine
) -> Bool {
  let str = String(bridged: input)
  let captureBuffer = UnsafeMutableRawBufferPointer(
    start: captureStructureOut,
    count: Int(captureStructureSize)
  )
  do {
    // FIXME: We need to plumb through the 'regexToEmit' result to the caller.
    // For now, it is the same as the input.
    let (_, version) = try swiftCompilerParseRegexLiteral(
      str,
      captureBufferOut: captureBuffer
    )
    versionOut.pointee = UInt(version)
    return false
  } catch let error as CompilerParseError {
    var diagLoc = bridgedDiagnosticBaseLoc
    if diagLoc.isValid, let errorLoc = error.location {
      let offset = str.utf8.distance(from: str.startIndex, to: errorLoc)
      diagLoc = diagLoc.advanced(by: offset)
    }
    var message = error.message
    message.withBridgedString { message in
      BridgedDiagnostic(
        at: diagLoc,
        message: message,
        severity: .error,
        engine: bridgedDiagnosticEngine
      ).finish()
    }
    return true
  } catch {
    fatalError("Expected CompilerParseError")
  }
}

#else  // canImport(_CompilerRegexParser)

#warning("Regex parsing is disabled")

#endif
