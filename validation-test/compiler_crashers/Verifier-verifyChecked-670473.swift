// {"kind":"typecheck","signature":"(anonymous namespace)::Verifier::verifyChecked(swift::VarDecl*)","signatureNext":"Verifier::walkToDeclPost"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b>(c: b = d) {
}
func d(c: e)
func d() {
}
