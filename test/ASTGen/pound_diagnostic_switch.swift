// RUN: %target-typecheck-verify-swift -enable-experimental-feature ParserASTGen

// REQUIRES: swift_feature_ParserASTGen

// '#warning'/'#error' are allowed in case position of a switch and are
// evaluated for their diagnostic, without being treated as a case.

func testPoundWarningInSwitch(_ x: Int) {
  switch x {
  #warning("warning in case position") // expected-warning {{warning in case position}}
  case 1:
    break
  default:
    break
  }
}

func testPoundErrorInSwitch(_ x: Int) {
  switch x {
  #error("error in case position") // expected-error {{error in case position}}
  case 1:
    break
  default:
    break
  }
}

func testPoundDiagnosticInSwitchIfConfig(_ x: Int) {
  switch x {
  #if true
  #warning("warning in #if") // expected-warning {{warning in #if}}
  #endif
  case 1:
    break
  default:
    break
  }
}

func testPoundDiagnosticInSwitchElse(_ x: Int) {
  switch x {
  #if false
  case 1:
    break
  #else
  #warning("warning in #else") // expected-warning {{warning in #else}}
  #endif
  default:
    break
  }
}

func testPoundDiagnosticInSwitchNestedIfConfig(_ x: Int) {
  switch x {
  #if true
  #if true
  #warning("warning in nested #if") // expected-warning {{warning in nested #if}}
  #endif
  #endif
  case 1:
    break
  default:
    break
  }
}
