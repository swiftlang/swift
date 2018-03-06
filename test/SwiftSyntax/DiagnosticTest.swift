// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: OS=macosx
// REQUIRES: objc_interop

import Foundation
import StdlibUnittest
import SwiftSyntax

func loc(_ file: String = #file, line: Int = #line,
         column: Int = #line) -> SourceLocation {
  return SourceLocation(line: line, column: column, offset: 0, file: file)
}

/// Adds static constants to Diagnostic.Message.
extension Diagnostic.Message {
  /// Error thrown when a conversion between two types is impossible.
  static func cannotConvert(fromType: String,
                            toType: String) -> Diagnostic.Message {
    return .init(.error,
      "cannot convert value of type '\(fromType)' to '\(toType)'")
  }

  /// Suggestion for the user to explicitly check a value does not equal zero.
  static let checkEqualToZero =
    Diagnostic.Message(.note, "check for explicit equality to '0'")
}

var Diagnostics = TestSuite("Diagnostics")

Diagnostics.test("DiagnosticEmission") {
  let startLoc = loc()
  let fixLoc = loc()

  let engine = DiagnosticEngine()
  expectFalse(engine.hasErrors)

  engine.diagnose(.cannotConvert(fromType: "Int", toType: "Bool"),
                  location: startLoc) {
    $0.note(.checkEqualToZero, location: fixLoc,
            fixIts: [.insert(fixLoc, " != 0")])
  }

  expectEqual(engine.diagnostics.count, 1)
  guard let diag = engine.diagnostics.first else { return }
  expectEqual(diag.message.text,
              "cannot convert value of type 'Int' to 'Bool'")
  expectEqual(diag.message.severity, .error)
  expectEqual(diag.notes.count, 1)
  expectTrue(engine.hasErrors)

  guard let note = diag.notes.first else { return }
  expectEqual(note.message.text, "check for explicit equality to '0'")
  expectEqual(note.message.severity, .note)
  expectEqual(note.fixIts.count, 1)

  guard let fixIt = note.fixIts.first else { return }
  expectEqual(fixIt.text, " != 0")
}

runAllTests()
