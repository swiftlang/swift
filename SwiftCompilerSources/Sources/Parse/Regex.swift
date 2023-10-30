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

import _RegexParserBridging
import AST
import Basic

#if canImport(_CompilerRegexParser)
@_spi(CompilerInterface) import _CompilerRegexParser

func registerRegexParser() {
  Parser_registerRegexLiteralParsingFn(_RegexLiteralParsingFn)
  Parser_registerRegexLiteralLexingFn(_RegexLiteralLexingFn)
}

/// Bridging between C++ lexer and swiftCompilerLexRegexLiteral.
///
/// Attempt to lex a regex literal string.
///
/// - Parameters:
///   - CurPtrPtr: A pointer to the current pointer of lexer, which should be
///                the start of the literal. This will be advanced to the point
///                at which the lexer should resume, or will remain the same if
///                this is not a regex literal.
///   - BufferEndPtr: A pointer to the end of the buffer, which should not be
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
private func _RegexLiteralLexingFn(
  _ curPtrPtr: UnsafeMutablePointer<UnsafePointer<CChar>>,
  _ bufferEndPtr: UnsafePointer<CChar>,
  _ mustBeRegex: CBool,
  _ bridgedDiagnosticEngine: BridgedNullableDiagnosticEngine
) -> /*CompletelyErroneous*/ CBool {
  let inputPtr = curPtrPtr.pointee

  guard let (resumePtr, error) = swiftCompilerLexRegexLiteral(
    start: inputPtr, bufferEnd: bufferEndPtr, mustBeRegex: mustBeRegex
  ) else {
    // Not a regex literal, fallback without advancing the pointer.
    return false
  }

  // Advance the current pointer.
  curPtrPtr.pointee = resumePtr.assumingMemoryBound(to: CChar.self)

  if let error = error {
    // Emit diagnostic if diagnostics are enabled.
    if let diagEngine = DiagnosticEngine(bridged: bridgedDiagnosticEngine) {
      let startLoc = SourceLoc(bridged: BridgedSourceLoc(raw: error.location))!
      diagEngine.diagnose(startLoc, .foreign_diagnostic, error.message)
    }
    return error.completelyErroneous
  }
  return false
}

/// Bridging between C++ parser and swiftCompilerParseRegexLiteral.
///
/// - Parameters:
///   - inputPtr: A null-terminated C string.
///   - errOut: A buffer accepting an error string upon error.
///   - versionOut: A buffer accepting a regex literal format
///     version.
///   - captureStructureOut: A buffer accepting a byte sequence representing the
///     capture structure.
///   - captureStructureSize: The size of the capture structure buffer. Must be
///     greater than or equal to `strlen(inputPtr)`.
///   - bridgedDiagnosticBaseLoc: Source location of the start of the literal
///   - bridgedDiagnosticEngine: Diagnostic engine to emit diagnostics.
///
/// - Returns: `true` if there was a parse error, `false` otherwise.
public func _RegexLiteralParsingFn(
  _ inputPtr: UnsafePointer<CChar>,
  _ versionOut: UnsafeMutablePointer<CUnsignedInt>,
  _ captureStructureOut: UnsafeMutableRawPointer,
  _ captureStructureSize: CUnsignedInt,
  _ bridgedDiagnosticBaseLoc: BridgedSourceLoc,
  _ bridgedDiagnosticEngine: BridgedDiagnosticEngine
) -> Bool {
  let str = String(cString: inputPtr)
  let captureBuffer = UnsafeMutableRawBufferPointer(
    start: captureStructureOut, count: Int(captureStructureSize))
  do {
    // FIXME: We need to plumb through the 'regexToEmit' result to the caller.
    // For now, it is the same as the input.
    let (_, version) = try swiftCompilerParseRegexLiteral(
      str, captureBufferOut: captureBuffer)
    versionOut.pointee = CUnsignedInt(version)
    return false
  } catch let error as CompilerParseError {
    var diagLoc = SourceLoc(bridged: bridgedDiagnosticBaseLoc)
    let diagEngine = DiagnosticEngine(bridged: bridgedDiagnosticEngine)
    if let _diagLoc = diagLoc, let errorLoc = error.location {
      let offset = str.utf8.distance(from: str.startIndex, to: errorLoc)
      diagLoc = _diagLoc.advanced(by: offset)
    }
    diagEngine.diagnose(diagLoc, .foreign_diagnostic, error.message)
    return true
  } catch {
    fatalError("Expected CompilerParseError")
  }
}

#else // canImport(_CompilerRegexParser)

#warning("Regex parsing is disabled")
func registerRegexParser() {}

#endif // canImport(_CompilerRegexParser)
