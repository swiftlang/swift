// {"kind":"typecheck","original":"4dcd6877","signature":"swift::Parser::parseStmtForEach(swift::LabeledStmtInfo)","signatureAssert":"Assertion failed: (InBindingPattern == PatternBindingState::NotInBinding && \"for-each loops cannot exist inside other patterns\"), function parseStmtForEach","signatureNext":"Parser::parseStmt"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
guard
  let if <#expression#> {
    for <#pattern#> in <#expression#> {
    }
  } else {
  }
