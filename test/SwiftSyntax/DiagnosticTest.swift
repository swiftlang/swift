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

func getInput(_ file: String) -> URL {
  var result = URL(fileURLWithPath: #file)
  result.deleteLastPathComponent()
  result.appendPathComponent("Inputs")
  result.appendPathComponent(file)
  return result
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

  static func badFunction(_ name: TokenSyntax) -> Diagnostic.Message {
    return .init(.error, "bad function '\(name.text)'")
  }
  static func endOfFunction(_ name: TokenSyntax) -> Diagnostic.Message {
    return .init(.warning, "end of function '\(name.text)'")
  }
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

Diagnostics.test("SourceLocations") {
  let engine = DiagnosticEngine()
  engine.addConsumer(PrintingDiagnosticConsumer())
  let url = getInput("diagnostics.swift")

  class Visitor: SyntaxVisitor {
    let url: URL
    let engine: DiagnosticEngine
    init(url: URL, engine: DiagnosticEngine) {
      self.url = url
      self.engine = engine
    }
    override func visit(_ function: FunctionDeclSyntax) {
      let startLoc = function.identifier.startLocation(in: url)
      let endLoc = function.endLocation(in: url)
      print("\(function.identifier.text): startLoc: \(startLoc), endLoc: \(endLoc)")
      engine.diagnose(.badFunction(function.identifier), location: startLoc) {
        $0.highlight(function.identifier.sourceRange(in: self.url))
      }
      engine.diagnose(.endOfFunction(function.identifier), location: endLoc)
    }
  }

  expectDoesNotThrow({
    let file = try SourceFileSyntax.parse(url)
    Visitor(url: url, engine: engine).visit(file)
  })

  expectEqual(6, engine.diagnostics.count)
  let lines = Set(engine.diagnostics.compactMap { $0.location?.line })
  expectEqual([1, 3, 5, 7, 9, 11], lines)
  let columns = Set(engine.diagnostics.compactMap { $0.location?.column })
  expectEqual([6, 2], columns)
}

runAllTests()
