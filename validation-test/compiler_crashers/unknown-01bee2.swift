// {"kind":"typecheck","original":"09e735b9","signature":"unknown"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a
  enum b<c: a {
    case (b<d<c>>)
    e{switch self{}}
  }
  struct d<c>: a
