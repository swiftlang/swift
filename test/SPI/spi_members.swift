// RUN: %target-typecheck-verify-swift

@propertyWrapper
public struct Wrapper<T> {
  public init(wrappedValue: T) {}

  public var wrappedValue: T { fatalError() }
}

@_spi(Foo)
public class Bar {
  // expected-note@-1 16{{type declared here}}

  public init() {}
}

public struct ResilientStructSPIMembers {
  public init() {}

  @_spi(Foo) public func method(_: Bar) {}
  @_spi(Foo) public var computedProperty: Bar { Bar() }

  @_spi(Foo) public var storedProperty1: Bar
  @_spi(Foo) public var storedProperty2 = Bar()
  @_spi(Foo) public lazy var lazyProperty1 = Bar()
  @_spi(Foo) public lazy var lazyProperty2: Bar = Bar()
  @_spi(Foo) @Wrapper public var wrappedProperty1: Bar
  @_spi(Foo) @Wrapper public var wrappedProperty2 = Bar()
}

@frozen public struct FrozenStructSPIMembers {
  public init() {}

  @_spi(Foo) public func method(_: Bar) {}
  @_spi(Foo) public var computedProperty: Bar { Bar() }

  @_spi(Foo) public var storedProperty1: Bar
  // expected-error@-1 {{cannot use class 'Bar' here; it is SPI}}
  // expected-error@-2 {{stored property 'storedProperty1' cannot be declared '@_spi' in a '@frozen' struct}}

  @_spi(Foo) public var storedProperty2 = Bar()
  // expected-error@-1 {{stored property 'storedProperty2' cannot be declared '@_spi' in a '@frozen' struct}}

  @_spi(Foo) public lazy var lazyProperty1 = Bar()
  // expected-error@-1 {{stored property 'lazyProperty1' cannot be declared '@_spi' in a '@frozen' struct}}

  @_spi(Foo) public lazy var lazyProperty2: Bar = Bar()
  // expected-error@-1 {{cannot use class 'Bar' here; it is SPI}}
  // expected-error@-2 {{stored property 'lazyProperty2' cannot be declared '@_spi' in a '@frozen' struct}}

  @_spi(Foo) @Wrapper public var wrappedProperty1: Bar
  // expected-error@-1 {{stored property 'wrappedProperty1' cannot be declared '@_spi' in a '@frozen' struct}}

  @_spi(Foo) @Wrapper public var wrappedProperty2 = Bar()
  // expected-error@-1 {{stored property 'wrappedProperty2' cannot be declared '@_spi' in a '@frozen' struct}}
}

@frozen public struct FrozenStructPublicMembers {
  public init() {}

  public func method(_: Bar) {} // expected-error {{cannot use class 'Bar' here; it is SPI}}

  public var storedProperty1: Bar
  // expected-error@-1 {{cannot use class 'Bar' here; it is SPI}}

  public var storedProperty2 = Bar()
  // expected-error@-1 {{cannot use class 'Bar' here; it is SPI}}
  // expected-error@-2 {{class 'Bar' cannot be used in a property initializer in a '@frozen' type because it is SPI}}
  // expected-error@-3 {{initializer 'init()' cannot be used in a property initializer in a '@frozen' type because it is SPI}}

  public var computedProperty: Bar { Bar() } // expected-error {{cannot use class 'Bar' here; it is SPI}}

  public lazy var lazyProperty1 = Bar() // expected-error {{cannot use class 'Bar' here; it is SPI}}

  public lazy var lazyProperty2: Bar = Bar() // expected-error {{cannot use class 'Bar' here; it is SPI}}

  @Wrapper public var wrappedProperty1: Bar
  // expected-error@-1 {{cannot use class 'Bar' here; it is SPI}}

  @Wrapper public var wrappedProperty2 = Bar()
  // expected-error@-1 {{cannot use class 'Bar' here; it is SPI}}
  // expected-error@-2 {{class 'Bar' cannot be used in a property initializer in a '@frozen' type because it is SPI}}
  // expected-error@-3 {{initializer 'init()' cannot be used in a property initializer in a '@frozen' type because it is SPI}}
}

@frozen public struct FrozenStructPrivateMembers {
  private init() {}

  private func method(_: Bar) {}

  private var storedProperty1: Bar
  // expected-error@-1 {{cannot use class 'Bar' here; it is SPI}}

  private var storedProperty2 = Bar()
  // expected-error@-1 {{cannot use class 'Bar' here; it is SPI}}
  // expected-error@-2 {{class 'Bar' cannot be used in a property initializer in a '@frozen' type because it is SPI}}
  // expected-error@-3 {{initializer 'init()' cannot be used in a property initializer in a '@frozen' type because it is SPI}}

  private var computedProperty: Bar { Bar() }

  private lazy var lazyProperty1 = Bar() // expected-error {{cannot use class 'Bar' here; it is SPI}}

  private lazy var lazyProperty2: Bar = Bar() // expected-error {{cannot use class 'Bar' here; it is SPI}}

  @Wrapper private var wrappedProperty1: Bar
  // expected-error@-1 {{cannot use class 'Bar' here; it is SPI}}

  @Wrapper private var wrappedProperty2 = Bar()
  // expected-error@-1 {{cannot use class 'Bar' here; it is SPI}}
  // expected-error@-2 {{class 'Bar' cannot be used in a property initializer in a '@frozen' type because it is SPI}}
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
