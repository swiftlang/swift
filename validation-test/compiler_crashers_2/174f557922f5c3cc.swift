// {"kind":"typecheck","original":"2fdf1565","signature":"(anonymous namespace)::TypeSubstituter::transformDependentMemberType(swift::DependentMemberType*, swift::TypePosition)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  typealias b = c
  associatedtype d: b
  associatedtype J: Sequence = e<Self> where J.Element == d.f
}
protocol c {
  associatedtype f
}
struct g<h>: a {
  struct d
  }
  struct e<i: a>: Sequence {
    struct Iterator: IteratorProtocol {
      typealias Element = i.d.f
