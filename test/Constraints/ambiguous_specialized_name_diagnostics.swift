// RUN: %empty-directory(%t/src)
// RUN: %empty-directory(%t/sdk)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend -emit-module %t/src/A.swift \
// RUN:   -module-name A -swift-version 5 -enable-library-evolution \
// RUN:   -emit-module-path %t/A.swiftmodule

// RUN: %target-swift-frontend -emit-module %t/src/B.swift \
// RUN:   -I %t -module-name B -swift-version 5 -enable-library-evolution \
// RUN:   -emit-module-path %t/B.swiftmodule

// RUN: %target-swift-frontend -typecheck %t/src/main.swift \
// RUN:   -module-name main -I %t -verify

// https://github.com/apple/swift/issues/67799

//--- A.swift
public final class S<T> {
  public init(t: T) {
  }

  public func test() {}
  public static func staticFn() {}
}

//--- B.swift
public final class S<T> {
  public init(t: T) {
  }

  public func test() {}
  public static func staticFn() {}
}

//--- main.swift
import A
import B

func test() {
  _ = S<Int>(t: 42) // expected-error {{ambiguous use of 'init(t:)'}}

  S<Int>(t: 42).test() // expected-error {{ambiguous use of 'init(t:)'}}

  S<Int>.staticFn()
  // expected-error@-1 {{ambiguous use of 'staticFn()'}}
}
