// {"kind":"typecheck","original":"8e10a6ec","signature":"(anonymous namespace)::Verifier::verifyChecked(swift::IdentityExpr*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b> {
}
func c<each d>(e: repeat a<each d>) {
  (try repeat each e)
}
