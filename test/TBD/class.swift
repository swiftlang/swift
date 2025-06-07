// REQUIRES: VENDOR=apple 
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s
// RUN: %target-swift-frontend -enable-library-evolution -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s -enable-testing
// RUN: %target-swift-frontend -enable-library-evolution -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s -enable-testing

// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing -O %s
// RUN: %target-swift-frontend -enable-library-evolution -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing -O %s
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s -enable-testing -O
// RUN: %target-swift-frontend -enable-library-evolution -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s -enable-testing -O

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -parse-as-library -module-name test %s -emit-tbd -emit-tbd-path %t/typecheck.tbd -tbd-install_name class
// RUN: %target-swift-frontend -emit-ir -parse-as-library -module-name test %s -emit-tbd -emit-tbd-path %t/emit-ir.tbd -tbd-install_name class
// RUN: %llvm-readtapi --compare %t/typecheck.tbd %t/emit-ir.tbd

open class OpenNothing {}

open class OpenInit {
    public init() {}
    public init(public_: Int, default_: Int = 0) {}

    internal init(internal_: Int, default_: Int = 0) {}

    deinit {}
}

open class OpenMethods {
    public init() {}
    public func publicMethod(default_: Int = 0) {}
    internal func internalMethod(default_: Int = 0) {}
    private func privateMethod(default_: Int = 0) {}
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
    public static func publicStaticFunc(default_: Int = 0) {}
    internal static func internalStaticFunc(default_: Int = 0) {}
    private static func privateStaticFunc(default_: Int = 0) {}

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
    public init(public_: Int, default_: Int = 0) {}
    
    internal init(internal_: Int, default_: Int = 0) {}

    deinit {}
}

public class PublicMethods {
    public init() {}
    public func publicMethod(default_: Int = 0) {}
    internal func internalMethod(default_: Int = 0) {}
    private func privateMethod(default_: Int = 0) {}
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
    public static func publicStaticFunc(default_: Int = 0) {}
    internal static func internalStaticFunc(default_: Int = 0) {}
    private static func privateStaticFunc(default_: Int = 0) {}

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

  public init<S>(t: T, u: U, v: V, _: S, default_: Int = 0) {
    publicVar = t
    internalVar = u
    privateVar = v
  }

  public func publicGeneric<A>(_: A, default_: Int = 0) {}
  internal func internalGeneric<A>(_: A, default_: Int = 0) {}
  private func privateGeneric<A>(_: A, default_: Int = 0) {}

  public static func publicStaticGeneric<A>(_: A, default_: Int = 0) {}
  internal static func internalStaticGeneric<A>(_: A, default_: Int = 0) {}
  private static func privateStaticGeneric<A>(_: A, default_: Int = 0) {}
}


internal class InternalNothing {}

internal class InternalInit {
    internal init() {}
    internal init(internal_: Int, default_: Int = 0) {}
    private init(private_: Int, default_: Int = 0) {}
}

internal class InternalMethods {
    internal init() {}
    internal func internalMethod(default_: Int = 0) {}
    private func privateMethod(default_: Int = 0) {}
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
    internal static func internalStaticFunc(default_: Int = 0) {}
    private static func privateStaticFunc(default_: Int = 0) {}

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

  internal init<S>(t: T, u: U, v: V, _: S, default_: Int = 0) {
    internalVar = u
    privateVar = v
  }

  internal func internalGeneric<A>(_: A, default_: Int = 0) {}
  private func privateGeneric<A>(_: A, default_: Int = 0) {}

  internal static func internalStaticGeneric<A>(_: A, default_: Int = 0) {}
  private static func privateStaticGeneric<A>(_: A, default_: Int = 0) {}
}


private class PrivateNothing {}

private class PrivateInit {
    private init() {}
    private init(private_: Int, default_: Int = 0) {}
}

private class PrivateMethods {
    private init() {}
    private func privateMethod(default_: Int = 0) {}
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
    private static func privateStaticFunc(default_: Int = 0) {}

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

  private init<S>(t: T, u: U, v: V, _: S, default_: Int = 0) {
    privateVar = v
  }

  private func privateGeneric<A>(_: A, default_: Int = 0) {}

  private static func privateStaticGeneric<A>(_: A, default_: Int = 0) {}
}

public class PublicDynamicMembers {
  public dynamic init() {}
  public dynamic convenience init(x: Int) {
    self.init()
  }
  public dynamic func dynamicMethod() {}
  public dynamic class func dynamicClassMethod() {}
}

extension PublicDynamicMembers {
  @_dynamicReplacement(for: init(x:))
  public convenience init(y: Int) {
    self.init()
  }

  @_dynamicReplacement(for: dynamicMethod())
  public func methodReplacement() {}

  @_dynamicReplacement(for: dynamicClassMethod())
  public class func classMethodReplacement() {}
}
