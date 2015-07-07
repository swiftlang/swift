// RUN: not %target-swift-frontend %s -parse

final class A<X, Y where Y.Z == X> {
  private var y: Y
}
