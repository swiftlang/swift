// {"kind":"typecheck","original":"4dcd6877","signature":"swift::Parser::parseStmtForEach(swift::LabeledStmtInfo)","signatureAssert":"Assertion failed: (InBindingPattern == PatternBindingState::NotInBinding && \"for-each loops cannot exist inside other patterns\"), function parseStmtForEach","signatureNext":"Parser::parseStmt"}
// RUN: not --crash %target-swift-frontend -typecheck %s
guard
  let if <#expression#> {
    for <#pattern#> in <#expression#> {
    }
  } else {
  }
