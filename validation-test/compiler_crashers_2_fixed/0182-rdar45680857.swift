// RUN: %target-swift-frontend -emit-ir %s

public protocol Graph: class, Collection {
    associatedtype V
    associatedtype E
}

public protocol EdgeContainer {
    associatedtype E
    associatedtype Visitor: NeighboursVisitor where Visitor.C == Self

    func push(_ thing: E)
}

public protocol NeighboursVisitor {
    associatedtype C: EdgeContainer
    associatedtype G: Graph where G.E == C.E
}

public enum LIFONeighboursVisitor<C: EdgeContainer, G: Graph>: NeighboursVisitor where G.E == C.E {

}

public class Stack<T, G: Graph>: EdgeContainer where T == G.E {
    public typealias E = T
    public typealias Visitor = LIFONeighboursVisitor<Stack<T, G>, G>

    public func push(_ thing: T) {  }
}
