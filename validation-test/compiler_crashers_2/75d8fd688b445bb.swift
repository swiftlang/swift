// {"kind":"typecheck","original":"b7a10f72","signature":"(anonymous namespace)::Verifier::walkToExprPre(swift::Expr*)","signatureAssert":"Assertion failed: ((HadError || !isa<SourceFile *>(M) || cast<SourceFile *>(M)->ASTStage < SourceFile::TypeChecked) && \"OverloadedDeclRef\" \"in wrong phase\"), function walkToExprPre"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a(b : Int) {
  _ = {
    switch b {
    case Optional<c>.d :
      break
    }
  }
}
