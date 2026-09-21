// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s
// RUN: %target-swift-emit-silgen %s
// RUN: %target-swift-emit-silgen %s -enable-library-evolution
// RUN: %target-swift-emit-silgen %s -enable-testing
// RUN: %target-swift-emit-silgen %s -enable-library-evolution -enable-testing

public struct Visitor<Node, Failure: Error> {
  public var visit: (Node) throws(Failure) -> Void
}
