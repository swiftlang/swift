// RUN: %target-typecheck-verify-swift

@available(*, deprecated: 0.1) // expected-warning {{unexpected version number in 'available' attribute for non-specific platform '*'}}
public typealias PublicAlias = Int
public var value1: PublicAlias { return 1 }

@available(*, deprecated: 0.1) // expected-warning {{unexpected version number in 'available' attribute for non-specific platform '*'}}
@available(iOS, deprecated: 99.9)
private typealias PrivateAlias = Int // expected-note {{type declared here}}
public var value2: PrivateAlias { return 1 } // expected-error {{variable cannot be declared public because its type uses a private type}}
