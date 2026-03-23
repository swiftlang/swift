// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %swift-interface-tool -action minimize-source %t/input.swift > %t/output.swift
// RUN: %diff %t/output.swift %t/expected.swift

//--- input.swift

/// Test access levels on stored properties.
public struct StoredProps {
  public var pubVar: Int = 1
  private var privVar: Int = 2
  internal var internalVar: Int = 3
  fileprivate var fpVar: Int = 4
  package var pkgVar: Int = 5
  var implicitVar: Int = 6

  public init() {}
}

/// Test access levels on computed properties.
public struct ComputedProps {
  public var pubComputed: Int { return 1 }
  private var privComputed: Int { return 2 }
  internal var internalComputed: Int { return 3 }
  fileprivate var fpComputed: Int { return 4 }
  package var pkgComputed: Int { return 5 }
  var implicitComputed: Int { return 6 }
}

/// Test @usableFromInline on properties.
public struct UsableFromInlineProps {
  @usableFromInline internal var ufiVar: Int = 10
  @usableFromInline internal var ufiComputed: Int { return 20 }

  public init() {}
}

/// Test @_spi on properties.
@_spi(Testing)
public struct SPIStruct {
  public var spiPubVar: Int = 1
  private var spiPrivVar: Int = 2

  public init() {}
}

/// Test open access on class properties.
open class OpenClass {
  open var openVar: Int = 1
  public var pubVar: Int = 2
  private var privVar: Int = 3
  internal var internalVar: Int = 4
  fileprivate var fpVar: Int = 5

  public init() {}
}

/// Test properties with getter/setter access control.
public struct MixedAccessProps {
  public private(set) var readOnly: Int = 0
  public internal(set) var pubInternalSet: Int = 0

  public init() {}
}

/// Test @inlinable computed property — body preserved.
public struct InlinableProps {
  @inlinable public var inlinableComputed: Int { return 42 }
  public var normalComputed: Int { return 42 }
}

/// Struct without explicit init — properties follow normal access-level rules
/// and initializers are stripped when type annotation is present.
public struct NoInitAllAccess {
  public var pubVar: Int = 1
  private var privVar: Int = 2
  internal var internalVar: Int = 3
  fileprivate var fpVar: Int = 4
  package var pkgVar: Int = 5
  var implicitVar: Int = 6
}

/// Same properties but with explicit init — same access-level rules apply.
public struct WithInitAllAccess {
  public var pubVar: Int = 1
  private var privVar: Int = 2
  internal var internalVar: Int = 3
  fileprivate var fpVar: Int = 4
  package var pkgVar: Int = 5
  var implicitVar: Int = 6

  public init() {}
}

//--- expected.swift

public struct StoredProps {
  public var pubVar: Int
  private var privVar: Int
  package var pkgVar: Int

  public init()
}

public struct ComputedProps {
  public var pubComputed: Int {
      get
  }
  private var privComputed: Int {
      get
  }
  package var pkgComputed: Int {
      get
  }
}

public struct UsableFromInlineProps {
  @usableFromInline internal var ufiVar: Int
  @usableFromInline internal var ufiComputed: Int {
      get
  }

  public init()
}

@_spi(Testing)
public struct SPIStruct {
  public var spiPubVar: Int
  private var spiPrivVar: Int

  public init()
}

open class OpenClass {
  open var openVar: Int
  public var pubVar: Int
  private var privVar: Int

  public init()
}

public struct MixedAccessProps {
  public private(set) var readOnly: Int
  public internal(set) var pubInternalSet: Int

  public init()
}

public struct InlinableProps {
  @inlinable public var inlinableComputed: Int {
      return 42
  }
  public var normalComputed: Int {
      get
  }
}

public struct NoInitAllAccess {
  public var pubVar: Int = 1
  private var privVar: Int = 2
  package var pkgVar: Int = 5
}

public struct WithInitAllAccess {
  public var pubVar: Int
  private var privVar: Int
  package var pkgVar: Int

  public init()
}

