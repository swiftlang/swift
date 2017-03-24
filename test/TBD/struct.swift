// FIXME: TBDGen is incorrect:
// RUN: not %target-swift-frontend -c -parse-as-library -module-name test -validate-tbd-against-ir %s > %t.log 2>&1
// RUN: diff %t.log %S/Inputs/struct.log

public struct Nothing {}

public struct Init {
    public init() {}
    public init(public_: Int) {}
    
    init(private_: Int) {}
}

public struct Methods {
    public init() {}
    public func publicMethod() {}
    func privateMethod() {}
}

public struct Properties {
    public let publicLet: Int = 0
    let privateLet: Int = 0

    public var publicVar: Int = 0
    var privateVar: Int = 0

    public var publicVarGet: Int { return 0 }
    var privateVarGet: Int { return 0 }

    public var publicVarGetSet: Int {
        get { return 0 }
        set {}
    }
    var privateVarGetSet: Int {
        get { return 0 }
        set {}
    }
}

public struct Statics {
    public static func publicStaticFunc() {}
    static func privateStaticFunc() {}

    public static let publicLet: Int = 0
    static let privateLet: Int = 0

    public static var publicVar: Int = 0
    static var privateVar: Int = 0

    public static var publicVarGet: Int { return 0 }
    static var privateVarGet: Int { return 0 }

    public static var publicVarGetSet: Int {
        get { return 0 }
        set {}
    }
    static var privateVarGetSet: Int {
        get { return 0 }
        set {}
    }
}


struct PrivateNothing {}

struct PrivateInit {
    init() {}
    init(private_: Int) {}
}

struct PrivateMethods {
    init() {}
    func privateMethod() {}
}

struct PrivateProperties {
    let privateLet: Int = 0

    var privateVar: Int = 0

    var privateVarGet: Int { return 0 }

    var privateVarGetSet: Int {
        get { return 0 }
        set {}
    }
}

struct PrivateStatics {
    static func privateStaticFunc() {}

    static let privateLet: Int = 0

    static var privateVar: Int = 0

    static var privateVarGet: Int { return 0 }

    static var privateVarGetSet: Int {
        get { return 0 }
        set {}
    }
}

