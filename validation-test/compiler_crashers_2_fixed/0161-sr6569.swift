// RUN: %target-typecheck-verify-swift

protocol P {
    associatedtype A: P
}

struct Type<Param> {}
extension Type: P where Param: P, Param.A == Type<Param> { // expected-error {{requirement involves recursion that is not currently supported}}
    typealias A = Param
}
