// {"kind":"typecheck","signature":"swift::QueryTypeSubstitutionMap::operator()(swift::SubstitutableType*) const","signatureNext":"InFlightSubstitution::substType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b> {
  typealias c<d> = b
}
func e()
: a.c<Int>
