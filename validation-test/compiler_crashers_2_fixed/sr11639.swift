// RUN: not %target-typecheck-verify-swift

protocol ProtocolA {
    associatedtype T1
}

struct S<T> : ProtocolA {
  typealias T1 = T
}

protocol ProtocolB: ProtocolA {
    associatedtype T2: ProtocolB where T2.T1 == T3.T1
    associatedtype X
    typealias T3 = S<X>
}

