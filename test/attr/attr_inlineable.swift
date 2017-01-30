// RUN: %target-typecheck-verify-swift -swift-version 4

@_inlineable struct TestInlineableStruct {}
// expected-error@-1 {{@_inlineable cannot be applied to this declaration}}

private func privateFunction() {}
// expected-note@-1 5{{global function 'privateFunction()' is not '@_versioned' or public}}
fileprivate func fileprivateFunction() {}
// expected-note@-1 5{{global function 'fileprivateFunction()' is not '@_versioned' or public}}
func internalFunction() {}
// expected-note@-1 5{{global function 'internalFunction()' is not '@_versioned' or public}}
@_versioned func versionedFunction() {}
public func publicFunction() {}

func internalIntFunction() -> Int {}
// expected-note@-1 2{{global function 'internalIntFunction()' is not '@_versioned' or public}}

private struct PrivateStruct {}
// expected-note@-1 3{{struct 'PrivateStruct' is not '@_versioned' or public}}
struct InternalStruct {}
// expected-note@-1 4{{struct 'InternalStruct' is not '@_versioned' or public}}
@_versioned struct VersionedStruct {
  @_versioned init() {}
}
public struct PublicStruct {
  public init() {}
}

public struct Struct {
  @_transparent
  public func publicTransparentMethod() {
    struct Nested {}
    // expected-error@-1 {{type 'Nested' cannot be nested inside a '@_transparent' function}}

    publicFunction()
    // OK
    versionedFunction()
    // OK
    internalFunction()
    // expected-error@-1 {{global function 'internalFunction()' is internal and cannot be referenced from a '@_transparent' function}}
    fileprivateFunction()
    // expected-error@-1 {{global function 'fileprivateFunction()' is fileprivate and cannot be referenced from a '@_transparent' function}}
    privateFunction()
    // expected-error@-1 {{global function 'privateFunction()' is private and cannot be referenced from a '@_transparent' function}}
  }

  @_inlineable
  public func publicInlineableMethod() {
    struct Nested {}
    // expected-error@-1 {{type 'Nested' cannot be nested inside an '@_inlineable' function}}

    let _: PublicStruct
    let _: VersionedStruct
    let _: InternalStruct
    // expected-error@-1 {{struct 'InternalStruct' is internal and cannot be referenced from an '@_inlineable' function}}
    let _: PrivateStruct
    // expected-error@-1 {{struct 'PrivateStruct' is private and cannot be referenced from an '@_inlineable' function}}

    let _ = PublicStruct.self
    let _ = VersionedStruct.self
    let _ = InternalStruct.self
    // expected-error@-1 {{struct 'InternalStruct' is internal and cannot be referenced from an '@_inlineable' function}}
    let _ = PrivateStruct.self
    // expected-error@-1 {{struct 'PrivateStruct' is private and cannot be referenced from an '@_inlineable' function}}

    let _ = PublicStruct()
    let _ = VersionedStruct()
    let _ = InternalStruct()
    // expected-error@-1 {{struct 'InternalStruct' is internal and cannot be referenced from an '@_inlineable' function}}
    let _ = PrivateStruct()
    // expected-error@-1 {{struct 'PrivateStruct' is private and cannot be referenced from an '@_inlineable' function}}
  }

  @inline(__always)
  public func publicInlineAlwaysMethod(x: Any) {
    struct Nested {}
    // expected-error@-1 {{type 'Nested' cannot be nested inside an '@inline(__always)' function}}

    switch x {
      case is InternalStruct:
      // expected-error@-1 {{struct 'InternalStruct' is internal and cannot be referenced from an '@inline(__always)' function}}
        _ = ()
    }
  }

  private func privateMethod() {}
  // expected-note@-1 {{instance method 'privateMethod()' is not '@_versioned' or public}}

  @_transparent
  @_versioned
  func versionedTransparentMethod() {
    struct Nested {}
    // expected-error@-1 {{type 'Nested' cannot be nested inside a '@_transparent' function}}
    privateMethod()
    // expected-error@-1 {{instance method 'privateMethod()' is private and cannot be referenced from a '@_transparent' function}}
  }

  @_inlineable
  @_versioned
  func versionedInlineableMethod() {
    struct Nested {}
    // expected-error@-1 {{type 'Nested' cannot be nested inside an '@_inlineable' function}}
  }

  @inline(__always)
  @_versioned
  func versionedInlineAlwaysMethod() {
    struct Nested {}
    // expected-error@-1 {{type 'Nested' cannot be nested inside an '@inline(__always)' function}}
  }

  @_transparent
  func internalTransparentMethod() {
    struct Nested {}
    // OK
  }

  @_inlineable
  func internalInlineableMethod() {
    struct Nested {}
    // OK
  }

  @inline(__always)
  func internalInlineAlwaysMethod() {
    struct Nested {}
    // OK
  }
}

func internalFunctionWithDefaultValue(
    x: Int = {
      struct Nested {}
      // OK

      publicFunction()
      // OK
      versionedFunction()
      // OK
      internalFunction()
      // OK
      fileprivateFunction()
      // OK
      privateFunction()
      // OK

      return 0
    }(),
    y: Int = internalIntFunction()) {}

@_versioned func versionedFunctionWithDefaultValue(
    x: Int = {
      struct Nested {}
      // expected-error@-1 {{type 'Nested' cannot be nested inside a default argument value}}

      // FIXME: Some errors below are diagnosed twice

      publicFunction()
      // OK
      versionedFunction()
      // OK
      internalFunction()
      // expected-error@-1 2{{global function 'internalFunction()' is internal and cannot be referenced from a default argument value}}
      fileprivateFunction()
      // expected-error@-1 2{{global function 'fileprivateFunction()' is fileprivate and cannot be referenced from a default argument value}}
      privateFunction()
      // expected-error@-1 2{{global function 'privateFunction()' is private and cannot be referenced from a default argument value}}

      return 0
    }(),
    y: Int = internalIntFunction()) {}
    // expected-error@-1 {{global function 'internalIntFunction()' is internal and cannot be referenced from a default argument value}}

public func publicFunctionWithDefaultValue(
    x: Int = {
      struct Nested {}
      // expected-error@-1 {{type 'Nested' cannot be nested inside a default argument value}}

      // FIXME: Some errors below are diagnosed twice

      publicFunction()
      // OK
      versionedFunction()
      // OK
      internalFunction()
      // expected-error@-1 2{{global function 'internalFunction()' is internal and cannot be referenced from a default argument value}}
      fileprivateFunction()
      // expected-error@-1 2{{global function 'fileprivateFunction()' is fileprivate and cannot be referenced from a default argument value}}
      privateFunction()
      // expected-error@-1 2{{global function 'privateFunction()' is private and cannot be referenced from a default argument value}}

      return 0
    }(),
    y: Int = internalIntFunction()) {}
    // expected-error@-1 {{global function 'internalIntFunction()' is internal and cannot be referenced from a default argument value}}

// Make sure protocol extension members can reference protocol requirements
// (which do not inherit the @_versioned attribute).
@_versioned
protocol VersionedProtocol {
  associatedtype T

  func requirement() -> T
}

extension VersionedProtocol {
  func internalMethod() {}
  // expected-note@-1 {{instance method 'internalMethod()' is not '@_versioned' or public}}

  @_inlineable
  @_versioned
  func versionedMethod() -> T {
    internalMethod()
    // expected-error@-1 {{instance method 'internalMethod()' is internal and cannot be referenced from an '@_inlineable' function}}

    return requirement()
  }
}

enum InternalEnum {
// expected-note@-1 2{{enum 'InternalEnum' is not '@_versioned' or public}}
  case apple
  case orange
}

@_inlineable public func usesInternalEnum() {
  _ = InternalEnum.apple
  // expected-error@-1 {{enum 'InternalEnum' is internal and cannot be referenced from an '@_inlineable' function}}
  let _: InternalEnum = .orange
  // expected-error@-1 {{enum 'InternalEnum' is internal and cannot be referenced from an '@_inlineable' function}}
}

@_versioned enum VersionedEnum {
  case apple
  case orange
  // FIXME: Should this be banned?
  case pear(InternalEnum)
  case persimmon(String)
}

@_inlineable public func usesVersionedEnum() {
  _ = VersionedEnum.apple
  let _: VersionedEnum = .orange
  _ = VersionedEnum.persimmon
}
