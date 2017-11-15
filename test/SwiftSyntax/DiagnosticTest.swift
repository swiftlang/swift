// RUN: %target-swift 2>&1 | %FileCheck %s

import StdlibUnittest

func loc(_ file: String = #file, line: Int = #line, 
         column: Int = #line) -> SourceLocation {
  return SourceLocation(file: file, line: line, column: column, offset: 0)
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

try runTool { engine in
  let startLoc = loc()
  let fixLoc = loc()

  // CHECK: error: cannot convert value of type 'Int' to 'Bool'
  // CHECK-NEXT: note: check for explicit equality to '0'
  engine.diagnose(.cannotConvert(fromType: "Int", toType: "Bool"),
                  location: startLoc) {
    $0.note(.checkEqualToZero, location: fixLoc,
            fixIts: [.insert(fixLoc, " != 0")])
  }
  return 0
}
