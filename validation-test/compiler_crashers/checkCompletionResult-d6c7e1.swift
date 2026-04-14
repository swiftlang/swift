// {"kind":"typecheck","original":"a4bcbc57","signature":"checkCompletionResult"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b : a
}
protocol c {
  associatedtype d : BinaryInteger
  associatedtype e : c
  func f < i
  : c where i.e ==
  Self>()
  struct g<h : a> : c {
    typealias d = Int
    typealias e = g<h.b>
    func f()
  }
}
