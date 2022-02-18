//===--- Regex.swift - Regex compilation bridging layer -------------------===//
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
//
// Bridging layer between 'string-processing' module and the compiler
//
//===----------------------------------------------------------------------===//

#if SWIFT_ENABLE_EXPERIMENTAL_STRING_PROCESSING

// FIXME: Currenly, 'swift-experimental-string-processing' sources are compiled
// with this file. Ideally, it should be built as an independent module, so it
// can be imported in this file.

// import _MatchingEngine
import ParseBridging
import AST
import Basic

public func registerRegexParser() {
  Parser_registerRegexLiteralParsingFn(libswiftParseRegexLiteral)
  Parser_registerRegexLiteralLexingFn(bridgingLexRegexLiteral)
}

func bridgingLexRegexLiteral(
  _ curPtrPtr: UnsafeMutablePointer<UnsafePointer<CChar>?>,
  _ bufferEndPtr: UnsafePointer<CChar>,
  _ bridgedDiagEngine: BridgedDiagnosticEngine) -> Bool {

  var errOut: UnsafePointer<CChar>? = nil;
  let completelyErroneous = libswiftLexRegexLiteral(curPtrPtr, bufferEndPtr, &errOut);

  // NOTE: Temporary diagnostics.
  if let errCString = errOut {
    let errStr = String(cString: errCString);

    let diagEngine = DiagnosticEngine(bridged: bridgedDiagEngine)
    let loc = SourceLoc(pointer: curPtrPtr.pointee)
    diagEngine.diagnose(loc, .regex_literal_parsing_error, [errStr],
                        highlight: nil, fixIts: [])
  }

  return completelyErroneous;
}

// -- Prototype pseudo implementation --
#if PROTOTYPE_IMPL
func bridgingLexRegexLiteral(
  _ curPtrPtr: UnsafeMutablePointer<UnsafePointer<CChar>?>?,
  _ bufferEndPtr: UnsafePointer<CChar>?,
  _ bridgedDiagEngine: BridgedDiagnosticEngine) {

  let source = Source(curPtrPtr, buffferEndPointer)
  let lexer = RegexLexer(source)

  let pos: SourcePosition? = lexer.lex()

  let completelyErroneous: Bool
  if let pos = pos {
    completelyErroneous = false
    curPtrPtr = pos.pointer
  } else {
    completelyErroneous = true
  }

  let diagEngine = DiagnosticEngine(bridged: bridgedDiagEngine)
  for diagnostic in lexer.diagnostics {
    let loc = getSourceLoc(diagnostic.sourceLocation)
    switch diagnostic {
      case .someError(let arg):
        diagEngine.diagnose(loc, .regex_lex_some_error, [arg])
      case .otherWarning:
        diagEngine.diagnose(loc, .regex_lex_other_warning, [])
    }
  }

  return completelyErroneous;
}
#endif

#endif // SWIFT_ENABLE_EXPERIMENTAL_STRING_PROCESSING
