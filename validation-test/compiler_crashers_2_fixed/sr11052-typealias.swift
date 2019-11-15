// RUN: %target-typecheck-verify-swift

protocol ProtoA {
    associatedtype AType1
}

protocol ProtoB {
    associatedtype AType2: ProtoA
    func protoFunc() -> AType2.AType1
}

extension ProtoB {
    typealias Alias = AType2.AType1
}

struct Concrete<AType2: ProtoA>: ProtoB {

    func concreteFunc() -> Alias {
        fatalError()
    }

    func protoFunc() -> Alias {
        fatalError()
    }
}
