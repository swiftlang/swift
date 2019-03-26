// RUN: %target-swift-frontend -emit-ir -primary-file %s

protocol ProtoA { }

protocol ProtoB {
    associatedtype T: ProtoA
    var stuff: Int { get }
}

extension ProtoB {
    var stuff: Int {
        return 0
    }
}

class Foo<T: ProtoA>: ProtoB { }
