// {"kind":"typecheck","original":"3c7f61b0","signature":"verifyRewriteRules"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b: c where b.d == Self
}
protocol c {
  associatedtype d
  associatedtype e: a
  associatedtype Dual: c where Dual.Dual == Self
}
struct f<g
  extension f: c where g: c, g.e == f
    extension f: a
    where
      g: a,
      g.b == f
    {
      typealias b =
