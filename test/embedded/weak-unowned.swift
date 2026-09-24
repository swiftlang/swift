// Only 64-bit targets have a weak refcount field to support 'weak' and
// 'unowned'. See weak-basic.swift and unowned-basic.swift for what IRGen emits
// there and what the runtime does with it.

// RUN: %target-swift-frontend -typecheck %s -target arm64-apple-none-macho -enable-experimental-feature Embedded -parse-stdlib -module-name Swift
// RUN: %target-swift-frontend -typecheck %s -target armv7-apple-none-macho -enable-experimental-feature Embedded -parse-stdlib -module-name Swift -verify

// REQUIRES: swift_feature_Embedded

// We use -parse-stdlib so we can target armv7 above. Bring a few items that
// MyStruct needs below.
precedencegroup AssignmentPrecedence { assignment: true }

public protocol ExpressibleByNilLiteral {
  init(nilLiteral: ())
}

public enum Optional<Wrapped>: ExpressibleByNilLiteral {
  case none
  case some(Wrapped)
  public init(nilLiteral: ()) { self = .none }
}

public class MyClass { }

public struct MyStruct {
  var normalVar: MyClass
  weak var weakVar: MyClass? // expected-error {{attribute 'weak' in Embedded Swift requires a 64-bit target}}
  unowned var unownedVar: MyClass // expected-error {{attribute 'unowned' in Embedded Swift requires a 64-bit target}}
  unowned(unsafe) var unownedUnsafe: MyClass
}
