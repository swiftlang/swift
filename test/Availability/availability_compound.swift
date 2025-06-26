// RUN: %target-typecheck-verify-swift -swift-version 4

public struct Pair<A, B> {}

public struct PublicStruct {
  public struct Inner {}
  @available(*, unavailable)
  internal struct Obsolete {} // expected-note * {{marked unavailable here}}
  // expected-note@-1 3{{type declared here}}
}

@available(*, unavailable, renamed: "PublicStruct")
public typealias ObsoleteAlias = PublicStruct // expected-note * {{marked unavailable here}}

public let a: ObsoleteAlias.Inner? // expected-error {{'ObsoleteAlias' has been renamed to 'PublicStruct'}}

public let b: ObsoleteAlias.Obsolete? // expected-error {{'ObsoleteAlias' has been renamed to 'PublicStruct'}}
// expected-error@-1 {{constant cannot be declared public because its type uses an internal type}}

public let c: Pair<ObsoleteAlias.Inner, PublicStruct.Obsolete>? // expected-error {{'ObsoleteAlias' has been renamed to 'PublicStruct'}}
// expected-error@-1 {{'Obsolete' is unavailable}}
// expected-error@-2 {{constant cannot be declared public because its type uses an internal type}}

public let c2: Pair<PublicStruct.Obsolete, ObsoleteAlias.Inner>? // expected-error {{'Obsolete' is unavailable}}
// expected-error@-1 {{'ObsoleteAlias' has been renamed to 'PublicStruct'}}
// expected-error@-2 {{constant cannot be declared public because its type uses an internal type}}

public let d: ObsoleteAlias? // expected-error {{'ObsoleteAlias' has been renamed to 'PublicStruct'}}
