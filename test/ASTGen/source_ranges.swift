// Source-range and AST-shape problems that made ASTGen crash inside the C++ AST
// consumers rather than in ASTGen itself.

// RUN: %empty-directory(%t)

// The switch case body's brace locations and implicit flag have to match the
// C++ parser's, so compare the dumped ASTs directly rather than just the
// diagnostics.
// RUN: %target-swift-frontend-dump-parse -enable-experimental-feature ParserASTGen \
// RUN:    | %sanitize-address > %t/astgen.ast
// RUN: %target-swift-frontend-dump-parse \
// RUN:    | %sanitize-address > %t/cpp-parser.ast
// RUN: %diff -u %t/astgen.ast %t/cpp-parser.ast

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

func switchCaseBodies(_ x: Int) {
  // An empty case body is the interesting one: with no elements and no brace
  // locations, the BraceStmt's end location is invalid while the 'case' keyword
  // is valid, and `CaseStmt::getLabelItemsRange()` then builds a half-valid
  // SourceRange, which asserts.
  switch x {
  case 0:
    break
  case 1:
    ()
  default:
    break
  }

  // Several statements, to check the non-empty body range too.
  switch x {
  case 0:
    let y = x
    _ = y
  default:
    break
  }
}

func nestedSwitch(_ x: Int, _ y: Int) {
  switch x {
  case 0:
    switch y {
    case 0:
      break
    default:
      break
    }
  default:
    break
  }
}
