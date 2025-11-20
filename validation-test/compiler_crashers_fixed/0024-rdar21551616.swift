// RUN: not %target-swift-frontend %s -typecheck

final class A<X, Y where Y.Z == X> {
  private var y: Y
}
