// FIXME: TBDGen is incorrect:
// RUN: not %target-swift-frontend -c -parse-as-library -module-name test -validate-tbd-against-ir %s > %t.log 2>&1
// RUN: diff %t.log %S/Inputs/protocol.log

public protocol Public {
    func publicMethod()
    associatedtype PublicAT
    var publicVarGet: Int { get }
    var publicVarGetSet: Int { get set }
}
protocol Internal {
    func internalMethod()
    associatedtype InternalAT
    var internalVarGet: Int { get }
    var internalVarGetSet: Int { get set }
}
private protocol Private {
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

public struct PublicInternalPublicStruct: Internal {
    public func internalMethod() {}
    public typealias InternalAT = Int
    public let internalVarGet: Int = 0
    public var internalVarGetSet: Int = 0
}
public struct PublicPrivatePublicStruct: Private {
    public func privateMethod() {}
    public typealias PrivateAT = Int
    public let privateVarGet: Int = 0
    public var privateVarGetSet: Int = 0
}

public struct PublicInternalInternalStruct: Internal {
    func internalMethod() {}
    typealias InternalAT = Int
    let internalVarGet: Int = 0
    var internalVarGetSet: Int = 0
}
public struct PublicPrivateInternalStruct: Private {
    func privateMethod() {}
    typealias PrivateAT = Int
    let privateVarGet: Int = 0
    var privateVarGetSet: Int = 0
}

public struct PublicPrivateFileprivateStruct: Private {
    fileprivate func privateMethod() {}
    fileprivate typealias PrivateAT = Int
    fileprivate let privateVarGet: Int = 0
    fileprivate var privateVarGetSet: Int = 0
}

struct InternalPublicInternalStruct: Public {
    func publicMethod() {}
    typealias PublicAT = Int
    let publicVarGet: Int = 0
    var publicVarGetSet: Int = 0
}

struct InternalInternalInternalStruct: Internal {
    func internalMethod() {}
    typealias InternalAT = Int
    let internalVarGet: Int = 0
    var internalVarGetSet: Int = 0
}

struct InternalPrivateInternalStruct: Private {
    func privateMethod() {}
    typealias PrivateAT = Int
    let privateVarGet: Int = 0
    var privateVarGetSet: Int = 0
}

struct InternalPrivateFileprivateStruct: Private {
    fileprivate func privateMethod() {}
    fileprivate typealias PrivateAT = Int
    fileprivate let privateVarGet: Int = 0
    fileprivate var privateVarGetSet: Int = 0
}

private struct PrivatePublicInternalStruct: Public {
    func publicMethod() {}
    typealias PublicAT = Int
    let publicVarGet: Int = 0
    var publicVarGetSet: Int = 0
}

private struct PrivateInternalInternalStruct: Internal {
    func internalMethod() {}
    typealias InternalAT = Int
    let internalVarGet: Int = 0
    var internalVarGetSet: Int = 0
}

private struct PrivatePrivateInternalStruct: Private {
    func privateMethod() {}
    typealias PrivateAT = Int
    let privateVarGet: Int = 0
    var privateVarGetSet: Int = 0
}

private struct PrivatePrivateFileprivateStruct: Private {
    fileprivate func privateMethod() {}
    fileprivate typealias PrivateAT = Int
    fileprivate let privateVarGet: Int = 0
    fileprivate var privateVarGetSet: Int = 0
}
