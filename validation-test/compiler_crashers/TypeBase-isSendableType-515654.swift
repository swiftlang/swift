// {"kind":"typecheck","original":"7517e5f3","signature":"swift::TypeBase::isSendableType()","signatureNext":"varIsSafeAcrossActors"}
// ASTGen builds a different AST for this input and does not reach the crash.
// Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b: ~Copyable>() {
  @_nonSendable class c {
    struct e {
      var cache: c
      let d = \cache
    }
  }
}
