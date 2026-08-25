// RUN: %target-typecheck-verify-swift -enable-experimental-feature Embedded

// REQUIRES: swift_feature_Embedded

public protocol ClassBound: AnyObject { func f() }
public protocol Other: AnyObject { func g() }

public class C: ClassBound, Other {
  public init() {}
  public func f() {}
  public func g() {}
}

public class D: C {}

public func castToClassBoundProtocol(_ e: any ClassBound) -> Bool {
  // expected-error@+1{{cannot perform a dynamic cast to a type involving protocol 'Other' in Embedded Swift}}
  return e is any Other
}

public func conditionalCast(_ e: any ClassBound) -> Bool {
  // expected-error@+1{{cannot perform a dynamic cast to a type involving protocol 'Other' in Embedded Swift}}
  if let o = e as? any Other { o.g(); return true }
  return false
}

public func forcedCast(_ e: any ClassBound) {
  // expected-error@+1{{cannot perform a dynamic cast to a type involving protocol 'Other' in Embedded Swift}}
  let o = e as! any Other
  o.g()
}

// Casting to a concrete class is still fine: that is a metadata-pointer
// comparison, which needs no conformance lookup.
public func downcastToClass(_ e: any ClassBound) -> Bool {
  if let c = e as? C { c.f(); return true }
  return false
}

public func classToSubclass(_ c: C) -> Bool {
  if let d = c as? D { d.f(); return true }
  return false
}

// An upcast is static, not a dynamic cast.
public func upcast(_ c: C) -> any ClassBound {
  return c as any ClassBound
}
