// RUN: %target-parse-verify-swift
// RUN: %target-swift-frontend -emit-silgen -o /dev/null %s

// This file tests that the AST produced after fixing accessibility warnings
// is valid according to SILGen and the verifiers.

public struct PublicStruct {
  public var publicVar = 0
}

internal struct InternalStruct {
  public var publicVar = 0 // expected-warning {{declaring a public var for an internal struct}} {{3-9=internal}}

  public private(set) var publicVarPrivateSet = 0 // expected-warning {{declaring a public var for an internal struct}} {{3-9=internal}}

  public public(set) var publicVarPublicSet = 0 // expected-warning {{declaring a public var for an internal struct}} {{3-9=internal}} {{10-22=}}

  public var publicVarGetOnly: Int { return 0 } // expected-warning {{declaring a public var for an internal struct}} {{3-9=internal}}

  public var publicVarGetSet: Int { get { return 0 } set {} } // expected-warning {{declaring a public var for an internal struct}} {{3-9=internal}}
}

private struct PrivateStruct {
  public var publicVar = 0 // expected-warning {{declaring a public var for a private struct}} {{3-9=private}}
}


extension PublicStruct {
  public init(x: Int) { self.init() }

  public var publicVarExtension: Int { get { return 0 } set {} }
}

extension InternalStruct {
  public init(x: Int) { self.init() } // expected-warning {{declaring a public initializer for an internal struct}} {{3-9=internal}}

  public var publicVarExtension: Int { get { return 0 } set {} } // expected-warning {{declaring a public var for an internal struct}} {{3-9=internal}}
}

extension PrivateStruct {
  public init(x: Int) { self.init() } // expected-warning {{declaring a public initializer for a private struct}} {{3-9=private}}

  public var publicVarExtension: Int { get { return 0 } set {} } // expected-warning {{declaring a public var for a private struct}} {{3-9=private}}
}

public extension PublicStruct {
  public func extMemberPublic() {}
  private func extImplPublic() {}
}
internal extension PublicStruct {
  public func extMemberInternal() {} // expected-warning {{declaring a public instance method in an internal extension}}
  private func extImplInternal() {}
}
private extension PublicStruct {
  public func extMemberPrivate() {} // expected-warning {{declaring a public instance method in a private extension}}
  private func extImplPrivate() {}
}

internal extension InternalStruct {
  public func extMemberInternal() {} // expected-warning {{declaring a public instance method in an internal extension}}
  private func extImplInternal() {}
}
private extension InternalStruct {
  public func extMemberPrivate() {} // expected-warning {{declaring a public instance method in a private extension}}
  private func extImplPrivate() {}
}


private extension PrivateStruct {
  public func extMemberPrivate() {} // expected-warning {{declaring a public instance method in a private extension}}
  private func extImplPrivate() {}
}

public protocol PublicReadOnlyOperations {
  var size: Int { get }
  subscript (Int) -> Int { get }
}

internal struct PrivateSettersForReadOnlyInternal : PublicReadOnlyOperations {
  public private(set) var size = 0 // expected-warning {{declaring a public var for an internal struct}}
  internal private(set) subscript (Int) -> Int { // no-warning
    get { return 42 }
    set {}
  }
}


public class PublicClass {
  public var publicVar = 0
}

internal class InternalClass {
  public var publicVar = 0 // expected-warning {{declaring a public var for an internal class}} {{3-9=internal}}

  public private(set) var publicVarPrivateSet = 0 // expected-warning {{declaring a public var for an internal class}} {{3-9=internal}}

  public public(set) var publicVarPublicSet = 0 // expected-warning {{declaring a public var for an internal class}} {{3-9=internal}}

  public var publicVarGetOnly: Int { return 0 } // expected-warning {{declaring a public var for an internal class}} {{3-9=internal}}

  public var publicVarGetSet: Int { get { return 0 } set {} } // expected-warning {{declaring a public var for an internal class}} {{3-9=internal}}
}

private class PrivateClass {
  public var publicVar = 0 // expected-warning {{declaring a public var for a private class}} {{3-9=private}}
}

