// RUN: %target-swift-frontend %s -emit-module

protocol P2 {
    func f1<T : P1>(_: T) -> T.T1
}

public struct S1<U> {}

protocol P1 {
    typealias T1 = S1<Self>
}
