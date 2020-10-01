// RUN: not %target-swift-frontend -typecheck %s

protocol P1: class {
    associatedtype P1P1: P1
    associatedtype P1AnyP2: AnyP2<P1P1>
    
    var anyP2: P1AnyP2? { get set }
}

protocol P2 {
    associatedtype P2P1: P1
}

final class AnyP2<AP2P1: P1>: P2 {
    typealias P2P1 = AP2P1
}
