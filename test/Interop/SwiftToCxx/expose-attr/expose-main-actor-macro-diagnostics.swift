// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple -plugin-path %swift-plugin-dir -parse-as-library -cxx-interoperability-mode=default

// REQUIRES: swift_swift_parser
// REQUIRES: concurrency

@_expose(Cxx)
public struct S {
  @_ExposeMainActorToCxx // expected-note {{in expansion of macro '_ExposeMainActorToCxx' on instance method 'method()' here}}
  @MainActor public func method() -> Int { 1 }

  @_ExposeMainActorToCxx // expected-note {{in expansion of macro '_ExposeMainActorToCxx' on property 'property' here}}
  @MainActor public var property: Int { 1 }

  @_ExposeMainActorToCxx // expected-note {{in expansion of macro '_ExposeMainActorToCxx' on initializer 'init()' here}}
  @MainActor public init() {}

  @_ExposeMainActorToCxx // expected-note {{in expansion of macro '_ExposeMainActorToCxx' on subscript 'subscript(_:)' here}}
  @MainActor public subscript(i: Int) -> Int { i }
}

// The thunks are only available in C++.
// expected-note@@__swiftmacro_4main1SV6method21_ExposeMainActorToCxxfMp_.swift:1:1 {{'_cxx_method()' was obsoleted in Swift 1}}
// expected-note@@__swiftmacro_4main1SV8property21_ExposeMainActorToCxxfMp_.swift:1:1 {{'_cxx_property' was obsoleted in Swift 1}}
// expected-note@@__swiftmacro_4main1SV4init21_ExposeMainActorToCxxfMp_.swift:1:1 {{'_cxx_init()' was obsoleted in Swift 1}}
// expected-note@@__swiftmacro_4main1SV9subscript21_ExposeMainActorToCxxfMp_.swift:1:1 {{'_cxx_subscript' was obsoleted in Swift 1}}
func callThunksFromSwift(_ s: S) {
  _ = s._cxx_method() // expected-error {{'_cxx_method()' is unavailable: only available in C++}}
  _ = s._cxx_property // expected-error {{'_cxx_property' is unavailable: only available in C++}}
  _ = S._cxx_init() // expected-error {{'_cxx_init()' is unavailable: only available in C++}}
  _ = s._cxx_subscript(0) // expected-error {{'_cxx_subscript' is unavailable: only available in C++}}
}

@_expose(Cxx)
public struct Unsupported {
  @_ExposeMainActorToCxx // expected-error@+1 {{does not support async declarations}}
  @MainActor public func asyncMethod() async {}

  @_ExposeMainActorToCxx // expected-error@+1 {{does not support rethrows}}
  @MainActor public func rethrowing(_ f: () throws -> Void) rethrows {}

  @_ExposeMainActorToCxx // expected-error@+1 {{does not support variadic parameters}}
  @MainActor public func variadic(_ x: Int...) {}

  @_ExposeMainActorToCxx // expected-error@+1 {{does not support autoclosure parameters}}
  @MainActor public func autoclosure(_ x: @autoclosure () -> Int) {}

  @_ExposeMainActorToCxx // expected-error@+1 {{does not support operators}}
  @MainActor public static func +(lhs: Unsupported, rhs: Unsupported) -> Unsupported { lhs }

  @_ExposeMainActorToCxx // expected-error@+1 {{does not support throwing accessors}}
  @MainActor public var throwingGetter: Int { get throws { 1 } }

  @_ExposeMainActorToCxx // expected-error {{property must have an explicit type annotation}}
  @MainActor public var inferred = 1

  @_ExposeMainActorToCxx("name") // expected-error {{a custom C++ name is only supported on functions and properties}}
  @MainActor public init() {}
}

public actor A {
  @_ExposeMainActorToCxx // expected-error {{does not support members of actors and protocols}}
  @MainActor public func method() {}
}

@_expose(Cxx)
public struct UnsupportedAccessors {
  public var x = 0
  @_ExposeMainActorToCxx // expected-error {{does not support mutating getters with nonmutating setters}}
  @MainActor public var both: Int {
    mutating get { x }
    nonmutating set {}
  }
}
