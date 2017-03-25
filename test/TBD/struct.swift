// FIXME: TBDGen is incorrect:
// RUN: not %target-swift-frontend -c -parse-as-library -module-name test -validate-tbd-against-ir %s > %t.log 2>&1
// RUN: diff %t.log %S/Inputs/struct.log

public struct PublicNothing {}

public struct PublicInit {
    public init() {}

    public init(public_: Int) {}
    internal init(internal_: Int) {}
    private init(private_: Int) {}
}

public struct PublicMethods {
    public init() {}
    public func publicMethod() {}
    internal func internalMethod() {}
    private func privateMethod() {}
}

public struct PublicProperties {
    public let publicLet: Int = 0
    internal let internalLet: Int = 0
    private let privateLet: Int = 0

    public var publicVar: Int = 0
    internal var internalVar: Int = 0
    private var privateVar: Int = 0

    public var publicVarGet: Int { return 0 }
    internal var internalVarGet: Int { return 0 }
    private var privateVarGet: Int { return 0 }

    public var publicVarGetSet: Int {
        get { return 0 }
        set {}
    }
    internal var internalVarGetSet: Int {
        get { return 0 }
        set {}
    }
    private var privateVarGetSet: Int {
        get { return 0 }
        set {}
    }
}

public struct PublicStatics {
    public static func publicStaticFunc() {}
    internal static func internalStaticFunc() {}
    private static func privateStaticFunc() {}

    public static let publicLet: Int = 0
    internal static let internalLet: Int = 0
    private static let privateLet: Int = 0

    public static var publicVar: Int = 0
    internal static var internalVar: Int = 0
    private static var privateVar: Int = 0

    public static var publicVarGet: Int { return 0 }
    internal static var internalVarGet: Int { return 0 }
    private static var privateVarGet: Int { return 0 }

    public static var publicVarGetSet: Int {
        get { return 0 }
        set {}
    }
    internal static var internalVarGetSet: Int {
        get { return 0 }
        set {}
    }
    private static var privateVarGetSet: Int {
        get { return 0 }
        set {}
    }
}


internal struct InternalNothing {}

internal struct InternalInit {
    internal init() {}

    internal init(internal_: Int) {}
    private init(private_: Int) {}
}

internal struct InternalMethods {
    internal init() {}
    internal func internalMethod() {}
    private func privateMethod() {}
}

internal struct InternalProperties {
    internal let internalLet: Int = 0
    private let privateLet: Int = 0

    internal var internalVar: Int = 0
    private var privateVar: Int = 0

    internal var internalVarGet: Int { return 0 }
    private var privateVarGet: Int { return 0 }

    internal var internalVarGetSet: Int {
        get { return 0 }
        set {}
    }
    private var privateVarGetSet: Int {
        get { return 0 }
        set {}
    }
}

internal struct InternalStatics {
    internal static func internalStaticFunc() {}
    private static func privateStaticFunc() {}

    internal static let internalLet: Int = 0
    private static let privateLet: Int = 0

    internal static var internalVar: Int = 0
    private static var privateVar: Int = 0

    internal static var internalVarGet: Int { return 0 }
    private static var privateVarGet: Int { return 0 }

    internal static var internalVarGetSet: Int {
        get { return 0 }
        set {}
    }
    private static var privateVarGetSet: Int {
        get { return 0 }
        set {}
    }
}

private struct PrivateNothing {}

private struct PrivateInit {
    private init() {}
    private init(private_: Int) {}
}

private struct PrivateMethods {
    private init() {}
    private func privateMethod() {}
}

private struct PrivateProperties {
    private let privateLet: Int = 0

    private var privateVar: Int = 0

    private var privateVarGet: Int { return 0 }

    private var privateVarGetSet: Int {
        get { return 0 }
        set {}
    }
}

private struct PrivateStatics {
    private static func privateStaticFunc() {}

    private static let privateLet: Int = 0

    private static var privateVar: Int = 0

    private static var privateVarGet: Int { return 0 }

    private static var privateVarGetSet: Int {
        get { return 0 }
        set {}
    }
}
