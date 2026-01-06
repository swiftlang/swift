// RUN: %target-typecheck-verify-swift

struct Box<T> {}

infix operator =*=
protocol P1 {
  associatedtype A
  static func =*= (x: Self, y: Box<A>)
}
class C1<A>: P1 {
  static func =*= (x: C1, y: Box<A>) {}
}
