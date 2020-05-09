// RUN: %target-swift-frontend -typecheck %S/Inputs/default-witness.swift -primary-file %s -enable-library-evolution

public protocol GraphType {
    func insert<U>(_: GraphNode<U>, _: GraphNode<U>)
}

public extension GraphType {
    func insert<N: Node>(_: N, _: GraphNode<N.Output>) {}
}

