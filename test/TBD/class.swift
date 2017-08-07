// RUN: %target-swift-frontend -emit-ir -o- -parse-as-library -module-name test -validate-tbd-against-ir=missing %s

open class OpenNothing {}

open class OpenInit {
    public init() {}
    public init(public_: Int) {}

    internal init(internal_: Int) {}

    deinit {}
}

open class OpenMethods {
    public init() {}
    public func publicMethod() {}
    internal func internalMethod() {}
    private func privateMethod() {}
}

open class OpenProperties {
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

open class OpenStatics {
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

open class OpenGeneric<T, U, V> {
  public var publicVar: T
  internal var internalVar: U
  private var privateVar: V

  public var publicVarConcrete: Int = 0
  internal var internalVarConcrete: Int = 0
  private var privateVarConcrete: Int = 0

  public init<S>(t: T, u: U, v: V, _: S) {
    publicVar = t
    internalVar = u
    privateVar = v
  }

  public func publicGeneric<A>(_: A) {}
  internal func internalGeneric<A>(_: A) {}
  private func privateGeneric<A>(_: A) {}

  public static func publicStaticGeneric<A>(_: A) {}
  internal static func internalStaticGeneric<A>(_: A) {}
  private static func privateStaticGeneric<A>(_: A) {}
}

public class PublicNothing {}

public class PublicInit {
    public init() {}
    public init(public_: Int) {}
    
    internal init(internal_: Int) {}

    deinit {}
}

public class PublicMethods {
    public init() {}
    public func publicMethod() {}
    internal func internalMethod() {}
    private func privateMethod() {}
}

public class PublicProperties {
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

public class PublicStatics {
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

public class PublicGeneric<T, U, V> {
  public var publicVar: T
  internal var internalVar: U
  private var privateVar: V

  public var publicVarConcrete: Int = 0
  internal var internalVarConcrete: Int = 0
  private var privateVarConcrete: Int = 0

  public init<S>(t: T, u: U, v: V, _: S) {
    publicVar = t
    internalVar = u
    privateVar = v
  }

  public func publicGeneric<A>(_: A) {}
  internal func internalGeneric<A>(_: A) {}
  private func privateGeneric<A>(_: A) {}

  public static func publicStaticGeneric<A>(_: A) {}
  internal static func internalStaticGeneric<A>(_: A) {}
  private static func privateStaticGeneric<A>(_: A) {}
}


internal class InternalNothing {}

internal class InternalInit {
    internal init() {}
    internal init(internal_: Int) {}
    private init(private_: Int) {}
}

internal class InternalMethods {
    internal init() {}
    internal func internalMethod() {}
    private func privateMethod() {}
}

internal class InternalProperties {
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

internal class InternalStatics {
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

internal class InternalGeneric<T, U, V> {
  internal var internalVar: U
  private var privateVar: V

  internal var internalVarConcrete: Int = 0
  private var privateVarConcrete: Int = 0

  internal init<S>(t: T, u: U, v: V, _: S) {
    internalVar = u
    privateVar = v
  }

  internal func internalGeneric<A>(_: A) {}
  private func privateGeneric<A>(_: A) {}

  internal static func internalStaticGeneric<A>(_: A) {}
  private static func privateStaticGeneric<A>(_: A) {}
}


private class PrivateNothing {}

private class PrivateInit {
    private init() {}
    private init(private_: Int) {}
}

private class PrivateMethods {
    private init() {}
    private func privateMethod() {}
}

private class PrivateProperties {
    private let privateLet: Int = 0

    private var privateVar: Int = 0

    private var privateVarGet: Int { return 0 }

    private var privateVarGetSet: Int {
        get { return 0 }
        set {}
    }
}

private class PrivateStatics {
    private static func privateStaticFunc() {}

    private static let privateLet: Int = 0

    private static var privateVar: Int = 0

    private static var privateVarGet: Int { return 0 }

    private static var privateVarGetSet: Int {
        get { return 0 }
        set {}
    }
}

private class PrivateGeneric<T, U, V> {
  private var privateVar: V

  private var privateVarConcrete: Int = 0

  private init<S>(t: T, u: U, v: V, _: S) {
    privateVar = v
  }

  private func privateGeneric<A>(_: A) {}

  private static func privateStaticGeneric<A>(_: A) {}
}
