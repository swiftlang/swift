// RUN: %target-typecheck-verify-swift

struct G1<T> {}

struct G2<A, B>: AP {
    func f1(_: G1<(B) -> A>, _: G1<B>) -> G1<A> { fatalError() }
    func f2<C>(_: (A) -> C) -> G1<C> { fatalError() }
}

protocol OP: EP {
    associatedtype L
    associatedtype R
    
    func f1(_: G1<L>, _: G1<R>) -> G1<A>
}

extension OP {
    func f1(_: G1<L>?, _: G1<R>?) -> G1<A> { fatalError() }
}

protocol AP: OP where L == (B) -> A, R == B {}

protocol EP {
    associatedtype A
    associatedtype B
    func f2<C>(_: (A) -> C) -> G1<C>
}
