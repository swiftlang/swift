// RUN: %target-typecheck-verify-swift -package-name myPkg

public class OuterClass {
  class InnerClass {}
  package class PkgInnerClass {}
}

public protocol PublicProto2 {
  associatedtype T
  associatedtype U
}

package class PkgOuterClass {
  class InnerClass {}
}

package protocol PackageProto {
  associatedtype T
  associatedtype U
}

extension PublicProto2 where Self.T : OuterClass, Self.U == OuterClass.InnerClass {
  public func cannotBePublic() {}
  // expected-error@-1 {{cannot declare a public instance method in an extension with internal requirements}}
}

package extension PublicProto2 where Self.T : OuterClass, Self.U == OuterClass.PkgInnerClass {
  public func cannotBePublic() {}
  // expected-error@-1 {{cannot declare a public instance method in an extension with package requirements}}
}

extension PackageProto where Self.T : OuterClass, Self.U == OuterClass.InnerClass {
  package func cannotBePublic() {}
  // expected-error@-1 {{cannot declare a package instance method in an extension with internal requirements}}
}

public extension OuterClass {
  open convenience init(x: ()) { self.init() }
  // expected-warning@-1 {{'open' modifier conflicts with extension's default access of 'public'}}
  // expected-error@-2 {{only classes and overridable class members can be declared 'open'; use 'public'}}
}

internal extension OuterClass {
  open convenience init(x: (), y: ()) { self.init() }
  // expected-warning@-1 {{'open' modifier conflicts with extension's default access of 'internal'}}
  // expected-error@-2 {{only classes and overridable class members can be declared 'open'; use 'public'}}
}

package extension OuterClass {
  open convenience init(x: (), y: (), z: ()) { self.init() }
  // expected-warning@-1 {{'open' modifier conflicts with extension's default access of 'package'}}
  // expected-error@-2 {{only classes and overridable class members can be declared 'open'; use 'public'}}
}

package extension PkgOuterClass {
  open convenience init(x: ()) { self.init() }
  // expected-warning@-1 {{'open' modifier conflicts with extension's default access of 'package'}}
  // expected-error@-2 {{only classes and overridable class members can be declared 'open'; use 'public'}}
}
