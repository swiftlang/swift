// RUN: %target-typecheck-verify-swift -parse-as-library

// REQUIRES: OS=macosx

@available(macOS, unavailable)
struct UnavailableMacOSStruct {} // expected-note 4 {{'UnavailableMacOSStruct' has been explicitly marked unavailable here}}

@available(iOS, introduced: 8.0)
@_spi_available(macOS, introduced: 10.9)
public struct SPIAvailableMacOSStruct {}

@available(*, unavailable)
public struct UniversallyUnavailableStruct {} // expected-note 3 {{'UniversallyUnavailableStruct' has been explicitly marked unavailable here}}

// Ok, initialization of globals is lazy and boxed.
@available(macOS, unavailable)
var unavailableMacOSGlobal: UnavailableMacOSStruct = .init()

@available(iOS, introduced: 8.0)
@_spi_available(macOS, introduced: 10.9)
var spiAvailableMacOSGlobal: SPIAvailableMacOSStruct = .init()

@available(*, unavailable)
var universallyUnavailableGlobal: UniversallyUnavailableStruct = .init()

@available(*, deprecated)
struct DeprecatedStruct {}

// Computed globals have no initial value to execute eagerly, so they are safe
// to mark unavailable.
@available(macOS, unavailable)
var computedUnavailableMacOSGlobal: UnavailableMacOSStruct { .init() }

@available(*, unavailable)
var computedUniversallyUnavailableGlobal: UniversallyUnavailableStruct { .init() }

struct GoodAvailableStruct {
  // Ok, computed property.
  @available(macOS, unavailable)
  var unavailableMacOS: UnavailableMacOSStruct {
    get { UnavailableMacOSStruct() }
  }

  // Ok, storage is available to implementation at runtime.
  @_spi_available(macOS, introduced: 10.9)
  var spiAvailableMacOS: SPIAvailableMacOSStruct

  // Ok, initialization of static vars is lazy and boxed.
  @available(macOS, unavailable)
  static var staticUnavailableMacOS: UnavailableMacOSStruct = .init()

  // Ok, an init accessor without an initial value has no initialization to run.
  @available(macOS, unavailable)
  var unavailableMacOSComputedWithInit: UnavailableMacOSStruct {
    init { _ = newValue }
    get { UnavailableMacOSStruct() }
  }

  // Ok, deprecation is advisory.
  @available(*, deprecated)
  var deprecated: DeprecatedStruct

  init() { fatalError() } // To suppress memberwise initializer
}

@available(macOS, unavailable)
struct GoodUnavailableMacOSStruct {
  var unavailableMacOS: UnavailableMacOSStruct = .init()
  lazy var lazyUnavailableMacOS: UnavailableMacOSStruct = .init()

  // Ok, the container is unavailable.
  @available(macOS, unavailable)
  var unavailableMacOSExplicit: UnavailableMacOSStruct = .init()

  // Ok, the container is unavailable.
  @available(macOS, unavailable)
  var unavailableMacOSComputedWithInitialValue: UnavailableMacOSStruct = .init() {
    init { _ = newValue }
    get { UnavailableMacOSStruct() }
  }
}

@available(macOS, unavailable)
struct GoodNestedUnavailableMacOSStruct {
  struct Inner {
    var unavailableMacOS: UnavailableMacOSStruct = .init()
    lazy var lazyUnavailableMacOS: UnavailableMacOSStruct = .init()

    // Ok, the container is unavailable.
    @available(macOS, unavailable)
    var unavailableMacOSExplicit: UnavailableMacOSStruct = .init()
  }
}

@available(*, unavailable)
struct GoodUniversallyUnavailableStruct {
  var universallyUnavailable: UniversallyUnavailableStruct = .init()
  lazy var lazyUniversallyUnavailable: UniversallyUnavailableStruct = .init()

  @available(*, unavailable)
  var universallyUnavailableExplicit: UniversallyUnavailableStruct = .init()
}

struct BadStruct {
  // expected-error@+1 {{stored properties cannot be marked unavailable with '@available'}}
  @available(macOS, unavailable)
  var unavailableMacOS: UnavailableMacOSStruct = .init() // expected-error {{'UnavailableMacOSStruct' is unavailable in macOS}}

  // expected-error@+1 {{stored properties cannot be marked unavailable with '@available'}}
  @available(macOS, unavailable)
  lazy var lazyUnavailableMacOS: UnavailableMacOSStruct = .init() // expected-error {{'UnavailableMacOSStruct' is unavailable in macOS}}

  // expected-error@+1 {{stored properties cannot be marked unavailable with '@available'}}
  @available(*, unavailable)
  var universallyUnavailable: UniversallyUnavailableStruct = .init() // expected-error {{'UniversallyUnavailableStruct' is unavailable}}

  // Ok, pure computed property.
  @available(macOS, unavailable)
  var computedUnavailableMacOS: UnavailableMacOSStruct { .init() }

  // Ok, init accessor without an initial value.
  @available(macOS, unavailable)
  var computedUnavailableMacOSWithInit: UnavailableMacOSStruct {
    init { _ = newValue }
    get { UnavailableMacOSStruct() }
  }

  // expected-error@+1 {{computed property with initial value cannot be marked unavailable with '@available'}}
  @available(macOS, unavailable)
  var computedUnavailableMacOSWithInitialValue: UnavailableMacOSStruct = .init() { // expected-error {{'UnavailableMacOSStruct' is unavailable in macOS}}
    init { _ = newValue }
    get { UnavailableMacOSStruct() } // expected-error {{'UnavailableMacOSStruct' is unavailable in macOS}}
  }

  // expected-error@+1 {{computed property with initial value cannot be marked unavailable with '@available'}}
  @available(*, unavailable)
  var computedUniversallyUnavailableWithInitialValue: UniversallyUnavailableStruct = .init() { // expected-error {{'UniversallyUnavailableStruct' is unavailable}}
    init { _ = newValue }
    get { UniversallyUnavailableStruct() } // expected-error {{'UniversallyUnavailableStruct' is unavailable}}
  }

  // Ok, deprecation is advisory.
  @available(*, deprecated)
  var deprecated: DeprecatedStruct

  init() { fatalError() } // To suppress memberwise initializer
}

enum GoodAvailableEnum {
  @available(macOS, unavailable)
  case unavailableMacOS(UnavailableMacOSStruct)

  @_spi_available(macOS, introduced: 10.9)
  case spiAvailableMacOS(SPIAvailableMacOSStruct)

  // Ok, deprecation is advisory.
  @available(*, deprecated)
  case deprecated(DeprecatedStruct)
}

struct SuppressedMemberwiseInit { // expected-error {{cannot automatically synthesize memberwise initializer for 'SuppressedMemberwiseInit'}}
  // Ok, computed properties without init accessors don't affect memberwise init.
  @available(macOS, unavailable)
  var computedUnavailableMacOS: UnavailableMacOSStruct {
    get { UnavailableMacOSStruct() }
    set { _ = newValue }
  }

  @available(macOS, unavailable)
  var computedUnavailableMacOSWithInit: UnavailableMacOSStruct { // expected-note {{unavailable property 'computedUnavailableMacOSWithInit' with init accessor prevents automatic synthesis of memberwise initializer}}
    init { _ = newValue }
    get { UnavailableMacOSStruct() }
  }

  @available(*, unavailable)
  var computedUniversallyUnavailableWithInit: UniversallyUnavailableStruct { // expected-note {{unavailable property 'computedUniversallyUnavailableWithInit' with init accessor prevents automatic synthesis of memberwise initializer}}
    init { _ = newValue }
    get { UniversallyUnavailableStruct() }
  }

  // Ok, deprecation is advisory.
  @available(*, deprecated)
  var deprecatedWithInit: DeprecatedStruct {
    init { _ = newValue }
    get { DeprecatedStruct() }
  }
}

@available(macOS, unavailable)
struct SuppressedMemberwiseInitUnavailableMacOS { // expected-error {{cannot automatically synthesize memberwise initializer for 'SuppressedMemberwiseInitUnavailableMacOS'}}
  // Ok, as available as struct.
  @available(macOS, unavailable)
  var computedUnavailableMacOSWithInit: UnavailableMacOSStruct {
    init { _ = newValue }
    get { UnavailableMacOSStruct() }
  }

  @available(*, unavailable)
  var computedUniversallyUnavailableWithInit: UniversallyUnavailableStruct { // expected-note {{unavailable property 'computedUniversallyUnavailableWithInit' with init accessor prevents automatic synthesis of memberwise initializer}}
    init { _ = newValue }
    get { UniversallyUnavailableStruct() }
  }
}

@available(macOS, unavailable)
struct HasMemberwiseInitUnavailableMacOS {
  // Ok, as available as struct.
  @available(macOS, unavailable)
  var computedUnavailableMacOSWithInit: UnavailableMacOSStruct {
    init { _ = newValue }
    get { UnavailableMacOSStruct() }
  }
}
