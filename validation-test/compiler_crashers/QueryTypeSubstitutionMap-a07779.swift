// {"kind":"typecheck","signature":"swift::QueryTypeSubstitutionMap::operator()(swift::SubstitutableType*) const","signatureNext":"InFlightSubstitution::substType"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b> {
  typealias c<d> = b
}
func e()
: a.c<Int>
