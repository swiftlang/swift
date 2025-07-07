// {"signature":"swift::QueryInterfaceTypeSubstitutions::operator()(swift::SubstitutableType*) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  typealias b = Int
}
protocol c {
  associatedtype b
}
extension a where b == Int
  extension a where Self : c {
    protocol Int : c {
d
