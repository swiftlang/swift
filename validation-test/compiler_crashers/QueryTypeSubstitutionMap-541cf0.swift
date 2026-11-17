// {"kind":"typecheck","signature":"swift::QueryTypeSubstitutionMap::operator()(swift::SubstitutableType*) const","signatureNext":"InFlightSubstitution::substType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a < b {
struct c < b{typealias d<e> = a} func f : c.d<Int>
