// {"kind":"typecheck","signature":"swift::QueryTypeSubstitutionMap::operator()(swift::SubstitutableType*) const","signatureNext":"InFlightSubstitution::substType"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b> {
  typealias c<d> = b
}
func e()
: a.c<Int>
