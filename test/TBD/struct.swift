// FIXME: TBDGen is incorrect:
// RUN: not %target-swift-frontend -c -parse-as-library -module-name test -validate-tbd-against-ir %s > %t.log 2>&1
// RUN: diff %t.log %S/Inputs/struct.log

public struct PublicNothing {}

public struct PublicInit {
    public init() {}
    public init(public_: Int) {}
    
    init(internal_: Int) {}
}

public struct PublicMethods {
    public init() {}
    public func publicMethod() {}
    func internalMethod() {}
}

public struct PublicProperties {
    public let publicLet: Int = 0
    let internalLet: Int = 0

    public var publicVar: Int = 0
    var internalVar: Int = 0

    public var publicVarGet: Int { return 0 }
    var internalVarGet: Int { return 0 }

    public var publicVarGetSet: Int {
        get { return 0 }
        set {}
    }
    var internalVarGetSet: Int {
        get { return 0 }
        set {}
    }
}

public struct PublicStatics {
    public static func publicStaticFunc() {}
    static func internalStaticFunc() {}

    public static let publicLet: Int = 0
    static let internalLet: Int = 0

    public static var publicVar: Int = 0
    static var internalVar: Int = 0

    public static var publicVarGet: Int { return 0 }
    static var internalVarGet: Int { return 0 }

    public static var publicVarGetSet: Int {
        get { return 0 }
        set {}
    }
    static var internalVarGetSet: Int {
        get { return 0 }
        set {}
    }
}


struct InternalNothing {}

struct InternalInit {
    init() {}
    init(internal_: Int) {}
}

struct InternalMethods {
    init() {}
    func internalMethod() {}
}

struct InternalProperties {
    let internalLet: Int = 0

    var internalVar: Int = 0

    var internalVarGet: Int { return 0 }

    var internalVarGetSet: Int {
        get { return 0 }
        set {}
    }
}

struct InternalStatics {
    static func internalStaticFunc() {}

    static let internalLet: Int = 0

    static var internalVar: Int = 0

    static var internalVarGet: Int { return 0 }

    static var internalVarGetSet: Int {
        get { return 0 }
        set {}
    }
}

