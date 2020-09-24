// Test the -require-explicit-availability flag
// REQUIRES: OS=macosx

// RUN: %swiftc_driver -typecheck -parse-as-library -target x86_64-apple-macosx10.10 -Xfrontend -verify -require-explicit-availability -require-explicit-availability-target "macOS 10.10"  %s

public struct S { // expected-warning {{public declarations should have an availability attribute when building with -require-explicit-availability}}
  public func method() { }
}

public func foo() { bar() } // expected-warning {{public declarations should have an availability attribute when building with -require-explicit-availability}} {{1-1=@available(macOS 10.10, *)\n}}

@usableFromInline
func bar() { } // expected-warning {{public declarations should have an availability attribute when building with -require-explicit-availability}} {{1-1=@available(macOS 10.10, *)\n}}

@available(macOS 10.1, *)
public func ok() { }

@available(macOS, unavailable)
public func unavailableOk() { }

@available(macOS, deprecated: 10.10)
public func missingIntro() { } // expected-warning {{public declarations should have an availability attribute when building with -require-explicit-availability}} {{1-1=@available(macOS 10.10, *)\n}}

@available(iOS 9.0, *)
public func missingTargetPlatform() { } // expected-warning {{public declarations should have an availability attribute when building with -require-explicit-availability}} {{1-1=@available(macOS 10.10, *)\n}}

func privateFunc() { }

@_alwaysEmitIntoClient
public func alwaysEmitted() { }

@available(macOS 10.1, *)
struct SOk {
  public func okMethod() { }
}

precedencegroup MediumPrecedence {}
infix operator + : MediumPrecedence

public func +(lhs: S, rhs: S) -> S { } // expected-warning {{public declarations should have an availability attribute when building with -require-explicit-availability}} {{1-1=@available(macOS 10.10, *)\n}}

public enum E { } // expected-warning {{public declarations should have an availability attribute when building with -require-explicit-availability}} {{1-1=@available(macOS 10.10, *)\n}}

public class C { } // expected-warning {{public declarations should have an availability attribute when building with -require-explicit-availability}} {{1-1=@available(macOS 10.10, *)\n}}

public protocol P { } // expected-warning {{public declarations should have an availability attribute when building with -require-explicit-availability}} {{1-1=@available(macOS 10.10, *)\n}}

private protocol PrivateProto { }

extension S { // expected-warning {{public declarations should have an availability attribute when building with -require-explicit-availability}} {{1-1=@available(macOS 10.10, *)\n}}
  public func warnForPublicMembers() { } // expected-warning {{public declarations should have an availability attribute when building with -require-explicit-availability}} {{3-3=@available(macOS 10.10, *)\n  }}
}

@available(macOS 10.1, *)
extension S {
  public func okWhenTheExtensionHasAttribute() { }
}

extension S {
  internal func dontWarnWithoutPublicMembers() { }
  private func dontWarnWithoutPublicMembers1() { }
}

extension S : P { // expected-warning {{public declarations should have an availability attribute when building with -require-explicit-availability}} {{1-1=@available(macOS 10.10, *)\n}}
}

extension S : PrivateProto {
}

open class OpenClass { } // expected-warning {{public declarations should have an availability attribute when building with -require-explicit-availability}} {{1-1=@available(macOS 10.10, *)\n}}

private class PrivateClass { }

extension PrivateClass { }

@available(macOS 10.1, *)
public protocol PublicProtocol { }

@available(macOS 10.1, *)
extension S : PublicProtocol { }

@_spi(SPIsAreOK)
public func spiFunc() { }

@_spi(SPIsAreOK)
public struct spiStruct {
  public func spiMethod() {}
}

extension spiStruct {
  public func spiExtensionMethod() {}
}

public var publicVar = S() // expected-warning {{public declarations should have an availability attribute when building with -require-explicit-availability}} {{1-1=@available(macOS 10.10, *)\n}}

@available(macOS 10.10, *)
public var publicVarOk = S()

public var (a, b) = (S(), S()) // expected-warning {{public declarations should have an availability attribute when building with -require-explicit-availability}} {{1-1=@available(macOS 10.10, *)\n}}

@available(macOS 10.10, *)
public var (c, d) = (S(), S())

public var _ = S() // expected-error {{global variable declaration does not bind any variables}}

public var implicitGet: S { // expected-warning {{public declarations should have an availability attribute when building with -require-explicit-availability}} {{1-1=@available(macOS 10.10, *)\n}}
  return S()
}

@available(macOS 10.10, *)
public var implicitGetOk: S {
  return S()
}

public var computed: S { // expected-warning {{public declarations should have an availability attribute when building with -require-explicit-availability}} {{1-1=@available(macOS 10.10, *)\n}}
  get { return S() }
  set { }
}

public var computedHalf: S { // expected-warning {{public declarations should have an availability attribute when building with -require-explicit-availability}} {{1-1=@available(macOS 10.10, *)\n}}
  @available(macOS 10.10, *)
  get { return S() }
  set { }
}

// FIXME the following warning is not needed.
public var computedOk: S { // expected-warning {{public declarations should have an availability attribute when building with -require-explicit-availability}} {{1-1=@available(macOS 10.10, *)\n}}
  @available(macOS 10.10, *)
  get { return S() }

  @available(macOS 10.10, *)
  set { }
}

@available(macOS 10.10, *)
public var computedOk1: S {
  get { return S() }
  set { }
}

public class SomeClass { // expected-warning {{public declarations should have an availability attribute when building with -require-explicit-availability}} {{1-1=@available(macOS 10.10, *)\n}}
  public init () {}

  public subscript(index: String) -> Int {
    get { return 42; }
    set(newValue) { }
  }
}

extension SomeClass { // expected-warning {{public declarations should have an availability attribute when building with -require-explicit-availability}} {{1-1=@available(macOS 10.10, *)\n}}
  public convenience init(s : S) {} // expected-warning {{public declarations should have an availability attribute when building with -require-explicit-availability}} {{3-3=@available(macOS 10.10, *)\n  }}

  @available(macOS 10.10, *)
  public convenience init(s : SomeClass) {}

  public subscript(index: Int) -> Int { // expected-warning {{public declarations should have an availability attribute when building with -require-explicit-availability}} {{3-3=@available(macOS 10.10, *)\n  }}
    get { return 42; }
    set(newValue) { }
  }

  @available(macOS 10.10, *)
  public subscript(index: S) -> Int {
    get { return 42; }
    set(newValue) { }
  }
}
