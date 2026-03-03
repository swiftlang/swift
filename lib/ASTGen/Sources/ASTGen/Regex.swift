//===--- Regex.swift ------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2025 Apple Inc. and the Swift project authors
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
          at: SourceLoc(raw: error.location),
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
  _ patternFeaturesOut: UnsafeMutablePointer<BridgedRegexLiteralPatternFeatures>,
  _ baseLoc: SourceLoc,
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
    // TODO: -> [Feature(opaque kind, (String.Index, length))]
    patternFeaturesOut.pointee = .init(baseAddress: nil, count: 0)
    versionOut.pointee = UInt(version)
    return false
  } catch let error as CompilerParseError {
    var diagLoc = baseLoc
    if diagLoc.isValid, let errorLoc = error.location {
      let offset = str.utf8.distance(from: str.startIndex, to: errorLoc)
      diagLoc = diagLoc.advanced(by: CInt(offset))
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

@_cdecl("swift_ASTGen_freeBridgedRegexLiteralPatternFeatures")
func freeBridgedRegexLiteralPatternFeatures(
  _ features: BridgedRegexLiteralPatternFeatures
) {
  let buffer = UnsafeMutableBufferPointer(
    start: features.getData(), count: features.getCount()
  )
  buffer.deinitialize()
  buffer.deallocate()
}

@_cdecl("swift_ASTGen_getSwiftVersionForRegexPatternFeature")
func getSwiftVersionForRegexPatternFeature(
  _ featureKind: BridgedRegexLiteralPatternFeatureKind,
  _ versionOut: UnsafeMutablePointer<BridgedSwiftVersion>
) {
  // TODO: FeatureKind(opaque kind) -> Version(major, minor)
  fatalError("Unimplemented")
}

@_cdecl("swift_ASTGen_getDescriptionForRegexPatternFeature")
func getDescriptionForRegexPatternFeature(
  _ featureKind: BridgedRegexLiteralPatternFeatureKind,
  _ context: BridgedASTContext,
  _ descriptionOut: UnsafeMutablePointer<BridgedStringRef>
) {
  // TODO: FeatureKind(opaque kind) -> String
  fatalError("Unimplemented")
}

#else  // canImport(_CompilerRegexParser)

#warning("Regex parsing is disabled")

#endif
