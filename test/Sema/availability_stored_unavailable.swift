// RUN: %target-typecheck-verify-swift -parse-as-library

// REQUIRES: OS=macosx

@available(macOS, unavailable)
struct UnavailableMacOSStruct {} // expected-note 2 {{'UnavailableMacOSStruct' has been explicitly marked unavailable here}}

@available(iOS, introduced: 8.0)
@_spi_available(macOS, introduced: 10.9)
public struct SPIAvailableMacOSStruct {}

@available(*, unavailable)
public struct UniversallyUnavailableStruct {} // expected-note {{'UniversallyUnavailableStruct' has been explicitly marked unavailable here}}

// Ok, initialization of globals is lazy and boxed.
@available(macOS, unavailable)
var unavailableMacOSGlobal: UnavailableMacOSStruct = .init()

@available(iOS, introduced: 8.0)
@_spi_available(macOS, introduced: 10.9)
var spiAvailableMacOSGlobal: SPIAvailableMacOSStruct = .init()

@available(*, unavailable)
var universallyUnavailableGlobal: UniversallyUnavailableStruct = .init()

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
}

@available(macOS, unavailable)
struct GoodUnavailableMacOSStruct {
  var unavailableMacOS: UnavailableMacOSStruct = .init()
  lazy var lazyUnavailableMacOS: UnavailableMacOSStruct = .init()

  // Ok, the container is unavailable.
  @available(macOS, unavailable)
  var unavailableMacOSExplicit: UnavailableMacOSStruct = .init()
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
}

enum GoodAvailableEnum {
  @available(macOS, unavailable)
  case unavailableMacOS(UnavailableMacOSStruct)

  @_spi_available(macOS, introduced: 10.9)
  case spiAvailableMacOS(SPIAvailableMacOSStruct)
}
