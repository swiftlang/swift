// FIXME: TBDGen is incorrect:
// RUN: not %target-swift-frontend -c -parse-as-library -module-name test -validate-tbd-against-ir %s > %t.out 2>&1
// RUN: diff %t.out %S/Inputs/protocol.log

public protocol Public {
    func publicMethod()
    associatedtype PublicAT
    var publicVarGet: Int { get }
    var publicVarGetSet: Int { get set }
}
protocol Private {
    func privateMethod()
    associatedtype PrivateAT
    var privateVarGet: Int { get }
    var privateVarGetSet: Int { get set }
}

// Naming scheme: type access, protocol access, witness access, type kind

public struct PublicPublicPublicStruct: Public {
    public func publicMethod() {}
    public typealias PublicAT = Int
    public let publicVarGet: Int = 0
    public var publicVarGetSet: Int = 0
}

public struct PublicPrivatePublicStruct: Private {
    public func privateMethod() {}
    public typealias PrivateAT = Int
    public let privateVarGet: Int = 0
    public var privateVarGetSet: Int = 0
}

public struct PublicPrivatePrivateStruct: Private {
    func privateMethod() {}
    typealias PrivateAT = Int
    let privateVarGet: Int = 0
    var privateVarGetSet: Int = 0
}

struct PrivatePublicPrivateStruct: Public {
    func publicMethod() {}
    typealias PublicAT = Int
    let publicVarGet: Int = 0
    var publicVarGetSet: Int = 0
}

struct PrivatePrivatePrivateStruct: Private {
    func privateMethod() {}
    typealias PrivateAT = Int
    let privateVarGet: Int = 0
    var privateVarGetSet: Int = 0
}
