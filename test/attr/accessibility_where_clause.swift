// RUN: %target-typecheck-verify-swift

public class OuterClass {
  class InnerClass {}
}

public protocol PublicProto2 {
  associatedtype T
  associatedtype U
}

// FIXME: With the current design, the below should not diagnose.
//
// However, it does, because we look at the bound decl in the
// TypeRepr first, and it happens to already be set.
//
// FIXME: Once we no longer do that, come up with another strategy
// to make the above diagnose.

extension PublicProto2 where Self.T : OuterClass, Self.U == Self.T.InnerClass {
  public func cannotBePublic() {}
  // expected-error@-1 {{cannot declare a public instance method in an extension with internal requirements}}
}

public extension OuterClass {
  open convenience init(x: ()) { self.init() }
  // expected-warning@-1 {{'open' modifier conflicts with extension's default access of 'public'}}
  // expected-error@-2 {{only classes and overridable class members can be declared 'open'; use 'public'}}
}
