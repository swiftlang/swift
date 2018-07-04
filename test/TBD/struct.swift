// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s -enable-resilience
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s -enable-testing
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s -enable-resilience -enable-testing
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s -O
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s -enable-resilience -O
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s -enable-testing -O
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s -enable-resilience -enable-testing -O

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

public struct PublicSubscripts {
    public subscript(publicGet _: Int) -> Int { return 0 }
    internal subscript(internalGet _: Int) -> Int { return 0 }
    private subscript(privateGet _: Int) -> Int { return 0 }

    public subscript(publicGetSet _: Int) -> Int {
        get {return 0 }
        set {}
    }
    internal subscript(internalGetSet _: Int) -> Int {
        get {return 0 }
        set {}
    }
    private subscript(privateGetSet _: Int) -> Int {
        get {return 0 }
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

public struct PublicGeneric<T, U, V> {
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

internal struct InternalSubscripts {
    internal subscript(internalGet _: Int) -> Int { return 0 }
    private subscript(privateGet _: Int) -> Int { return 0 }

    internal subscript(internalGetSet _: Int) -> Int {
        get {return 0 }
        set {}
    }
    private subscript(privateGetSet _: Int) -> Int {
        get {return 0 }
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

internal struct InternalGeneric<T, U, V> {
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

private struct PrivateSubscripts {
    private subscript(privateGet _: Int) -> Int { return 0 }

    private subscript(privateGetSet _: Int) -> Int {
        get {return 0 }
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

private struct PrivateGeneric<T, U, V> {
  private var privateVar: V

  private var privateVarConcrete: Int = 0

  private init<S>(t: T, u: U, v: V, _: S) {
    privateVar = v
  }

  private func privateGeneric<A>(_: A) {}

  private static func privateStaticGeneric<A>(_: A) {}
}
