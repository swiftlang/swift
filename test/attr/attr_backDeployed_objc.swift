// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -verify %s

// REQUIRES: OS=macosx

import Foundation

public class SwiftClass {
  @backDeployed(before: macOS 12.0) // expected-error {{'@backDeployed' must not be used on an '@objc' instance method}}
  @objc final public func objCMethod() {}
}

public class ObjCClass: NSObject {
  @backDeployed(before: macOS 12.0) // expected-error {{'@backDeployed' cannot be applied to a non-final instance method}}
  public func method() {}

  @backDeployed(before: macOS 12.0) // expected-error {{'@backDeployed' must not be used on an '@objc' instance method}}
  @objc public func objcMethod() {}

  @backDeployed(before: macOS 12.0) // expected-error {{'@backDeployed' must not be used on an '@objc' instance method}}
  @objc final public func finalObjcMethod() {}

  @backDeployed(before: macOS 12.0)
  final public func finalMethod() {}

  @backDeployed(before: macOS 12.0) // expected-error {{'@backDeployed' cannot be applied to a non-final instance method}}
  @nonobjc public func nonObjCMethod() {}

  @backDeployed(before: macOS 12.0)
  @nonobjc final public func finalNonObjCMethod() {}

  @backDeployed(before: macOS 12.0) // expected-error {{'@backDeployed' cannot be applied to a non-final property}}
  public var property: [Int] { [1] }

  @backDeployed(before: macOS 12.0) // expected-error {{'@backDeployed' must not be used on an '@objc' property}}
  @objc public var objCProperty: [Int] { [1] }

  @backDeployed(before: macOS 12.0) // expected-error {{'@backDeployed' must not be used on an '@objc' property}}
  @objc public final var finalObjCProperty: [Int] { [1] }

  @backDeployed(before: macOS 12.0)
  public final var finalProperty: [Int] { [1] }

  @backDeployed(before: macOS 12.0) // expected-error {{'@backDeployed' cannot be applied to a non-final property}}
  @nonobjc public var nonObjCProperty: [Int] { [1] }

  @backDeployed(before: macOS 12.0)
  @nonobjc public final var finalNonObjCProperty: [Int] { [1] }
}

extension DummyClass {
  @backDeployed(before: macOS 12.0) // expected-error {{'@backDeployed' cannot be applied to a non-final instance method}}
  public func method() {}

  @backDeployed(before: macOS 12.0) // expected-error {{'@backDeployed' must not be used on an '@objc' instance method}}
  @objc public func objcMethod() {}

  @backDeployed(before: macOS 12.0) // expected-error {{'@backDeployed' must not be used on an '@objc' instance method}}
  @objc final public func finalObjcMethod() {}

  @backDeployed(before: macOS 12.0)
  final public func finalMethod() {}

  @backDeployed(before: macOS 12.0) // expected-error {{'@backDeployed' cannot be applied to a non-final instance method}}
  @nonobjc public func nonObjCMethod() {}

  @backDeployed(before: macOS 12.0)
  @nonobjc final public func finalNonObjCMethod() {}

  @backDeployed(before: macOS 12.0) // expected-error {{'@backDeployed' cannot be applied to a non-final property}}
  public var property: [Int] { [1] }

  @backDeployed(before: macOS 12.0) // expected-error {{'@backDeployed' must not be used on an '@objc' property}}
  @objc public var objCProperty: [Int] { [1] }

  @backDeployed(before: macOS 12.0) // expected-error {{'@backDeployed' must not be used on an '@objc' property}}
  @objc public final var finalObjCProperty: [Int] { [1] }

  @backDeployed(before: macOS 12.0)
  public final var finalProperty: [Int] { [1] }

  @backDeployed(before: macOS 12.0) // expected-error {{'@backDeployed' cannot be applied to a non-final property}}
  @nonobjc public var nonObjCProperty: [Int] { [1] }

  @backDeployed(before: macOS 12.0)
  @nonobjc public final var finalNonObjCProperty: [Int] { [1] }
}
