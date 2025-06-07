// REQUIRES: VENDOR=apple 
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -disable-objc-attr-requires-foundation-module -validate-tbd-against-ir=all %s
// RUN: %target-swift-frontend -enable-library-evolution -emit-ir -o/dev/null -parse-as-library -module-name test -disable-objc-attr-requires-foundation-module -validate-tbd-against-ir=all %s
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -disable-objc-attr-requires-foundation-module -validate-tbd-against-ir=all %s -enable-testing
// RUN: %target-swift-frontend -enable-library-evolution -emit-ir -o/dev/null -parse-as-library -module-name test -disable-objc-attr-requires-foundation-module -validate-tbd-against-ir=all %s -enable-testing

// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -disable-objc-attr-requires-foundation-module -validate-tbd-against-ir=missing %s -O
// RUN: %target-swift-frontend -enable-library-evolution -emit-ir -o/dev/null -parse-as-library -module-name test -disable-objc-attr-requires-foundation-module -validate-tbd-against-ir=missing %s -O
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -disable-objc-attr-requires-foundation-module -validate-tbd-against-ir=missing %s -enable-testing -O
// RUN: %target-swift-frontend -enable-library-evolution -emit-ir -o/dev/null -parse-as-library -module-name test -disable-objc-attr-requires-foundation-module -validate-tbd-against-ir=missing %s -enable-testing -O

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -parse-as-library -module-name test -disable-objc-attr-requires-foundation-module %s -emit-tbd -emit-tbd-path %t/typecheck.tbd -tbd-install_name test
// RUN: %target-swift-frontend -emit-ir -parse-as-library -module-name test -disable-objc-attr-requires-foundation-module %s -emit-tbd -emit-tbd-path %t/emit-ir.tbd -tbd-install_name test
// RUN: %llvm-readtapi --compare  %t/typecheck.tbd %t/emit-ir.tbd

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

@_marker public protocol PublicMarker {}
@_marker internal protocol InternalMarker {}
@_marker private protocol PrivateMarker {}

@objc public protocol PublicObjc {}
@objc internal protocol InternalObjc {}
@objc private protocol PrivateObjc {}

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
