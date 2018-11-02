// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -enable-testing

@usableFromInline private func privateVersioned() {}
// expected-error@-1 {{'@usableFromInline' attribute can only be applied to internal declarations, but 'privateVersioned()' is private}}

@usableFromInline fileprivate func fileprivateVersioned() {}
// expected-error@-1 {{'@usableFromInline' attribute can only be applied to internal declarations, but 'fileprivateVersioned()' is fileprivate}}

@usableFromInline internal func internalVersioned() {}
// OK

@usableFromInline func implicitInternalVersioned() {}
// OK

@usableFromInline public func publicVersioned() {}
// expected-error@-1 {{'@usableFromInline' attribute can only be applied to internal declarations, but 'publicVersioned()' is public}}

internal class internalClass {
  @usableFromInline public func publicVersioned() {}
  // expected-error@-1 {{'@usableFromInline' attribute can only be applied to internal declarations, but 'publicVersioned()' is public}}
}

fileprivate class filePrivateClass {
  @usableFromInline internal func internalVersioned() {}
}

@usableFromInline struct S {
  var x: Int
  @usableFromInline var y: Int
}

@usableFromInline extension S {}
// expected-error@-1 {{'@usableFromInline' attribute cannot be applied to this declaration}}

@usableFromInline
protocol VersionedProtocol {
  associatedtype T

  func requirement() -> T

  public func publicRequirement() -> T
  // expected-error@-1 {{'public' modifier cannot be used in protocols}}
  // expected-note@-2 {{protocol requirements implicitly have the same access as the protocol itself}}

  @usableFromInline func versionedRequirement() -> T
  // expected-error@-1 {{'@usableFromInline' attribute cannot be used in protocols}}
}

// Derived conformances had issues with @usableFromInline - rdar://problem/34342955
@usableFromInline
internal enum EqEnum {
  case foo
}

@usableFromInline
internal enum RawEnum : Int {
  case foo = 0
}

@inlinable
public func usesEqEnum() -> Bool {
  _ = (EqEnum.foo == .foo)
  _ = EqEnum.foo.hashValue

  _ = RawEnum.foo.rawValue
  _ = RawEnum(rawValue: 0)
}
