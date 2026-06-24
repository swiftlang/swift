// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -typecheck -verify -primary-file %t/caller.swift %t/defaulted.swift

// REQUIRES: swift_feature_DefaultIsolationPerFile

//--- defaulted.swift
using @available(*, deprecated, message: "legacy")

public func defaultedFunc() {}

public var defaultedVar: Int = 42
public let defaultedLet: String = ""

public typealias DefaultedAlias = Int

public struct DefaultedStruct {}
public enum DefaultedEnum { case a }

public class DefaultedClass {
  public func method() {}
  public init() {}
}

extension DefaultedClass {
  public func extensionMethod() {}
}

@available(*, deprecated, message: "explicit")
public func explicitlyDeprecated() {}

//--- caller.swift
public func uses() {
  defaultedFunc() // expected-warning {{'defaultedFunc()' is deprecated: legacy}}

  _ = defaultedVar // expected-warning {{'defaultedVar' is deprecated: legacy}}
  _ = defaultedLet // expected-warning {{'defaultedLet' is deprecated: legacy}}

  let _: DefaultedAlias = 0 // expected-warning {{'DefaultedAlias' is deprecated: legacy}}

  _ = DefaultedStruct() // expected-warning {{'DefaultedStruct' is deprecated: legacy}}
  _ = DefaultedEnum.a // expected-warning {{'DefaultedEnum' is deprecated: legacy}}

  let c = DefaultedClass() // expected-warning {{'DefaultedClass' is deprecated: legacy}}
  c.method() // TODO: It would be ideal for deprecation to extend to members.
  c.extensionMethod() // expected-warning {{'extensionMethod()' is deprecated: legacy}}

  // Explicit per decl wins.
  explicitlyDeprecated() // expected-warning {{'explicitlyDeprecated()' is deprecated: explicit}}
}
