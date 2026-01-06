// RUN: not %target-swift-frontend -typecheck %s

struct A<X> {}
extension A<A<Int>.B> {
  struct B {}
}
