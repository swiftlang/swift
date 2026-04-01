// RUN: %target-swift-frontend -emit-ir %s

protocol Prot {
    var prop: String { get }
}

class Gen<A: Prot> { }

class Real: Gen<Real.RealProt> {
    enum RealProt: Prot {
        case first

        var prop: String {
            return "hello"
        }
    }
}
