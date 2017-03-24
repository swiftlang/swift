// FIXME: TBDGen is incorrect:
// RUN: not %target-swift-frontend -c -parse-as-library -module-name test -validate-tbd-against-ir %s > %t.log 2>&1
// RUN: diff %t.log %S/Inputs/class.log

public class PublicNothing {}

public class PublicInit {
    public init() {}
    public init(public_: Int) {}
    
    init(internal_: Int) {}

    deinit {}
}

public class PublicMethods {
    public init() {}
    public func publicMethod() {}
    func internalMethod() {}
}

public class PublicProperties {
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

public class PublicStatics {
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


class InternalNothing {}

class InternalInit {
    init() {}
    init(internal_: Int) {}
}

class InternalMethods {
    init() {}
    func internalMethod() {}
}

class InternalProperties {
    let internalLet: Int = 0

    var internalVar: Int = 0

    var internalVarGet: Int { return 0 }

    var internalVarGetSet: Int {
        get { return 0 }
        set {}
    }
}

class InternalStatics {
    static func internalStaticFunc() {}

    static let internalLet: Int = 0

    static var internalVar: Int = 0

    static var internalVarGet: Int { return 0 }

    static var internalVarGetSet: Int {
        get { return 0 }
        set {}
    }
}

