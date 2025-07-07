// {"signature":"swift::TypeBase::getReducedShape()"}
// RUN: not --crash %target-swift-frontend -typecheck %s
typealias a <b> = ()
extension a {
c {
  d
}
d
