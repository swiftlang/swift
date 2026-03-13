// RUN: %target-typecheck-verify-swift

protocol P1 {}

protocol P2 {
    associatedtype U
}

protocol P3<T> {
    associatedtype T: P2
}

struct S1<T: P2>: P3 where T.U: P1 {}

extension P3 where Self == S1<T> {  // expected-error {{same-type constraint 'Self' == 'S1<Self.T>' is recursive}}
    static var a: S1<T> {
        S1<T>()
    }
}
