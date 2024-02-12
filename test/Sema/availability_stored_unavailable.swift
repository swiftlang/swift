// RUN: %target-typecheck-verify-swift -parse-as-library

// REQUIRES: OS=macosx

@available(macOS, unavailable)
struct UnavailableMacOSStruct {}

@available(*, unavailable)
public struct UniversallyUnavailableStruct {}

// Ok, initialization of globals is lazy and boxed.
@available(macOS, unavailable)
var unavailableMacOSGlobal: UnavailableMacOSStruct = .init()

@available(*, unavailable)
var universallyUnavailableGlobal: UniversallyUnavailableStruct = .init()

struct GoodAvailableStruct {
  // Ok, computed property.
  @available(macOS, unavailable)
  var unavailableMacOS: UnavailableMacOSStruct {
    get { UnavailableMacOSStruct() }
  }

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
  var unavailableMacOS: UnavailableMacOSStruct = .init()

  // expected-error@+1 {{stored properties cannot be marked unavailable with '@available'}}
  @available(macOS, unavailable)
  lazy var lazyUnavailableMacOS: UnavailableMacOSStruct = .init()

  // expected-error@+1 {{stored properties cannot be marked unavailable with '@available'}}
  @available(*, unavailable)
  var universallyUnavailable: UniversallyUnavailableStruct = .init()
}

enum GoodAvailableEnum {
  @available(macOS, unavailable)
  case unavailableMacOS(UnavailableMacOSStruct)
}
