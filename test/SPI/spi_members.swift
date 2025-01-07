// RUN: %target-typecheck-verify-swift

@propertyWrapper
public struct Wrapper<T> {
  public init(wrappedValue: T) {}

  public var wrappedValue: T { fatalError() }
}

@_spi(Foo)
public class SPIType {
  // expected-note@-1 16{{class declared here}}

  public init() {}
}

public struct ResilientStructSPIMembers {
  public init() {}

  @_spi(Foo) public func method(_: SPIType) {}
  @_spi(Foo) public var computedProperty: SPIType { SPIType() }

  @_spi(Foo) public var storedProperty1: SPIType
  @_spi(Foo) public var storedProperty2 = SPIType()
  @_spi(Foo) public lazy var lazyProperty1 = SPIType()
  @_spi(Foo) public lazy var lazyProperty2: SPIType = SPIType()
  @_spi(Foo) @Wrapper public var wrappedProperty1: SPIType
  @_spi(Foo) @Wrapper public var wrappedProperty2 = SPIType()
}

@frozen public struct FrozenStructSPIMembers {
  public init() {}

  @_spi(Foo) public func method(_: SPIType) {}
  @_spi(Foo) public var computedProperty: SPIType { SPIType() }

  @_spi(Foo) public var storedProperty1: SPIType
  // expected-error@-1 {{cannot use class 'SPIType' here; it is SPI}}
  // expected-error@-2 {{stored property 'storedProperty1' cannot be declared '@_spi' in a '@frozen' struct}}

  @_spi(Foo) public var storedProperty2 = SPIType()
  // expected-error@-1 {{stored property 'storedProperty2' cannot be declared '@_spi' in a '@frozen' struct}}

  @_spi(Foo) public lazy var lazyProperty1 = SPIType()
  // expected-error@-1 {{stored property 'lazyProperty1' cannot be declared '@_spi' in a '@frozen' struct}}

  @_spi(Foo) public lazy var lazyProperty2: SPIType = SPIType()
  // expected-error@-1 {{cannot use class 'SPIType' here; it is SPI}}
  // expected-error@-2 {{stored property 'lazyProperty2' cannot be declared '@_spi' in a '@frozen' struct}}

  @_spi(Foo) @Wrapper public var wrappedProperty1: SPIType
  // expected-error@-1 {{stored property 'wrappedProperty1' cannot be declared '@_spi' in a '@frozen' struct}}

  @_spi(Foo) @Wrapper public var wrappedProperty2 = SPIType()
  // expected-error@-1 {{stored property 'wrappedProperty2' cannot be declared '@_spi' in a '@frozen' struct}}
}

@frozen public struct FrozenStructPublicMembers {
  public init() {}

  public func method(_: SPIType) {} // expected-error {{cannot use class 'SPIType' here; it is SPI}}

  public var storedProperty1: SPIType
  // expected-error@-1 {{cannot use class 'SPIType' here; it is SPI}}

  public var storedProperty2 = SPIType()
  // expected-error@-1 {{cannot use class 'SPIType' here; it is SPI}}
  // expected-error@-2 {{class 'SPIType' cannot be used in a property initializer in a '@frozen' type because it is SPI}}
  // expected-error@-3 {{initializer 'init()' cannot be used in a property initializer in a '@frozen' type because it is SPI}}

  public var computedProperty: SPIType { SPIType() } // expected-error {{cannot use class 'SPIType' here; it is SPI}}

  public lazy var lazyProperty1 = SPIType() // expected-error {{cannot use class 'SPIType' here; it is SPI}}

  public lazy var lazyProperty2: SPIType = SPIType() // expected-error {{cannot use class 'SPIType' here; it is SPI}}

  @Wrapper public var wrappedProperty1: SPIType
  // expected-error@-1 {{cannot use class 'SPIType' here; it is SPI}}

  @Wrapper public var wrappedProperty2 = SPIType()
  // expected-error@-1 {{cannot use class 'SPIType' here; it is SPI}}
  // expected-error@-2 {{class 'SPIType' cannot be used in a property initializer in a '@frozen' type because it is SPI}}
  // expected-error@-3 {{initializer 'init()' cannot be used in a property initializer in a '@frozen' type because it is SPI}}
}

@frozen public struct FrozenStructPrivateMembers {
  private init() {}

  private func method(_: SPIType) {}

  private var storedProperty1: SPIType
  // expected-error@-1 {{cannot use class 'SPIType' here; it is SPI}}

  private var storedProperty2 = SPIType()
  // expected-error@-1 {{cannot use class 'SPIType' here; it is SPI}}
  // expected-error@-2 {{class 'SPIType' cannot be used in a property initializer in a '@frozen' type because it is SPI}}
  // expected-error@-3 {{initializer 'init()' cannot be used in a property initializer in a '@frozen' type because it is SPI}}

  private var computedProperty: SPIType { SPIType() }

  private lazy var lazyProperty1 = SPIType() // expected-error {{cannot use class 'SPIType' here; it is SPI}}

  private lazy var lazyProperty2: SPIType = SPIType() // expected-error {{cannot use class 'SPIType' here; it is SPI}}

  @Wrapper private var wrappedProperty1: SPIType
  // expected-error@-1 {{cannot use class 'SPIType' here; it is SPI}}

  @Wrapper private var wrappedProperty2 = SPIType()
  // expected-error@-1 {{cannot use class 'SPIType' here; it is SPI}}
  // expected-error@-2 {{class 'SPIType' cannot be used in a property initializer in a '@frozen' type because it is SPI}}
  // expected-error@-3 {{initializer 'init()' cannot be used in a property initializer in a '@frozen' type because it is SPI}}
}

public enum ResilientEnum {
  @_spi(S)
  case okSpiCase

  @_spi(S)
  case okSpiCaseWithPayload(_: Int)
}

@frozen
public enum FrozenEnum {
  case okCase

  @_spi(S) // expected-error {{enum case 'spiCase' cannot be declared '@_spi' in a '@frozen' enum}}
  case spiCase

  case okCaseWithPayload(_: Int)

  @_spi(S) // expected-error {{enum case 'spiCaseWithPayload' cannot be declared '@_spi' in a '@frozen' enum}}
  case spiCaseWithPayload(_: Int)
}
