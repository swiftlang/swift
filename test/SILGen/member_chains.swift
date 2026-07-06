// RUN: %target-swift-emit-silgen -verify %s

struct ImplicitMembers: Equatable {
  static var implicit = ImplicitMembers()

  static var optional: ImplicitMembers? = ImplicitMembers()
  static func createOptional() -> ImplicitMembers? {
    ImplicitMembers()
  }

  var another: ImplicitMembers { ImplicitMembers() }
  var anotherOptional: ImplicitMembers? { ImplicitMembers() }
}

// Make sure we can SILGen these without issue.

postfix operator ^
postfix func ^ (_ lhs: ImplicitMembers) -> Int { 0 }

extension Int {
  func foo() {}
  var optionalMember: Int? { 0 }
}

// https://github.com/swiftlang/swift/issues/80265
// Make sure optional chaining looks through postfix operators.
var x: ImplicitMembers?
let _ = x?^.foo()
let _ = x?^.optionalMember?.foo()
let _ = x?.another^.optionalMember?.foo()

// Make sure the unresolved member chain extends up to the postfix operator,
// but the optional chain covers the entire expr.
let _ = .optional?^.foo()
let _ = .createOptional()?^.foo()
let _ = .implicit.anotherOptional?^.foo()
