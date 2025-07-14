// {"kind":"typecheck","original":"b7a10f72","signature":"(anonymous namespace)::Verifier::walkToExprPre(swift::Expr*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a(b : Int) {
  _ = {
    switch b {
    case Optional<c>.d :
      break
    }
  }
}
