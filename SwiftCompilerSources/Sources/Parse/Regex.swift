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
import _CompilerRegexParser

public func registerRegexParser() {
  Parser_registerRegexLiteralParsingFn(_RegexLiteralParsingFn)
  Parser_registerRegexLiteralLexingFn(_RegexLiteralLexingFn)
}

/// Bridging between C++ lexer and _CompilerRegexParser.lexRegex()
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
  _ bridgedDiagnosticEngine: BridgedOptionalDiagnosticEngine
) -> /*CompletelyErroneous*/ CBool {
  let inputPtr = curPtrPtr.pointee

  do {
    let (_, _, endPtr) = try lexRegex(start: inputPtr, end: bufferEndPtr)
    curPtrPtr.pointee = endPtr.assumingMemoryBound(to: CChar.self)
    return false
  } catch let error as DelimiterLexError {
    if !mustBeRegex {
      // This token can be something else. Let the client fallback.
      return false;
    }
    if error.kind == .unknownDelimiter {
      // An unknown delimiter should be recovered from, as we may want to try
      // lex something else.
      return false
    }

    if let diagEngine = DiagnosticEngine(bridged: bridgedDiagnosticEngine) {
      // Emit diagnostic.
      let startLoc = SourceLoc(
        locationInFile: UnsafeRawPointer(inputPtr).assumingMemoryBound(to: UInt8.self))!
      diagEngine.diagnose(startLoc, .regex_literal_parsing_error, "\(error)")
    }

    // Advance the current pointer.
    curPtrPtr.pointee = error.resumePtr.assumingMemoryBound(to: CChar.self)

    switch error.kind {
    case .unterminated, .multilineClosingNotOnNewline:
      // These can be recovered from.
      return false
    case .unprintableASCII, .invalidUTF8:
      // We don't currently have good recovery behavior for these.
      return true
    case .unknownDelimiter:
      fatalError("Already handled")
    }
  } catch {
    fatalError("Should be a DelimiterLexError")
  }
}

/// Bridging between C++ parser and _CompilerRegexParser.parseWithDelimiters()
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
public func _RegexLiteralParsingFn(
  _ inputPtr: UnsafePointer<CChar>,
  _ versionOut: UnsafeMutablePointer<CUnsignedInt>,
  _ captureStructureOut: UnsafeMutableRawPointer,
  _ captureStructureSize: CUnsignedInt,
  _ bridgedDiagnosticBaseLoc: BridgedSourceLoc,
  _ bridgedDiagnosticEngine: BridgedDiagnosticEngine
) -> Bool {
  versionOut.pointee = currentRegexLiteralFormatVersion

  let str = String(cString: inputPtr)
  do {
    let ast = try parseWithDelimiters(str)
    // Serialize the capture structure for later type inference.
    assert(captureStructureSize >= str.utf8.count)
    let buffer = UnsafeMutableRawBufferPointer(
        start: captureStructureOut, count: Int(captureStructureSize))
    ast.captureStructure.encode(to: buffer)
    return false;
  } catch {
    var diagLoc = SourceLoc(bridged: bridgedDiagnosticBaseLoc)
    let diagEngine = DiagnosticEngine(bridged: bridgedDiagnosticEngine)
    if let _diagLoc = diagLoc,
       let locatedError = error as? LocatedErrorProtocol {
      let offset = str.utf8.distance(from: str.startIndex,
                                     to: locatedError.location.start)
      diagLoc = _diagLoc.advanced(by: offset)
    }
    diagEngine.diagnose(
      diagLoc, .regex_literal_parsing_error,
      "cannot parse regular expression: \(String(describing: error))")
    return true
  }
}

#else // canImport(_CompilerRegexParser)

#warning("Regex parsing is disabled")
public func registerRegexParser() {}

#endif // canImport(_CompilerRegexParser)
