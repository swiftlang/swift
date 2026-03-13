// RUN: %target-typecheck-verify-swift -package-name myPkg
// RUN: %target-swift-frontend -typecheck -disable-access-control -package-name myPkg %s

public protocol ProtoWithReqs {
  associatedtype AssocA // expected-note * {{type declared here}}
  associatedtype AssocB // expected-note * {{type declared here}}
  func foo()
}

public struct Adopter<T> : ProtoWithReqs {}
// expected-error@-1 {{method 'foo()' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
// expected-error@-2 {{type alias 'AssocA' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
// expected-error@-3 {{type alias 'AssocB' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
extension Adopter {
  typealias AssocA = Int
  // expected-note@-1 {{mark the type alias as 'public' to satisfy the requirement}} {{3-3=public }}
  typealias AssocB = String
  // expected-note@-1 {{mark the type alias as 'public' to satisfy the requirement}} {{3-3=public }}
  func foo() {}
  // expected-note@-1 {{mark the instance method as 'public' to satisfy the requirement}} {{3-3=public }}
}

public struct Adopter2<T> : ProtoWithReqs {}
// expected-error@-1 {{method 'foo()' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
// expected-error@-2 {{type alias 'AssocA' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
// expected-error@-3 {{type alias 'AssocB' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
public extension Adopter2 {
  internal typealias AssocA = Int
  // expected-note@-1 {{mark the type alias as 'public' to satisfy the requirement}} {{3-12=}}
  package typealias AssocB = String
  // expected-note@-1 {{mark the type alias as 'public' to satisfy the requirement}} {{3-11=}}
  fileprivate func foo() {}
  // expected-note@-1 {{mark the instance method as 'public' to satisfy the requirement}} {{3-15=}}
}

public struct Adopter3<T> : ProtoWithReqs {}
// expected-error@-1 {{method 'foo()' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
// expected-error@-2 {{type alias 'AssocA' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
// expected-error@-3 {{type alias 'AssocB' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
internal extension Adopter3 {
  typealias AssocA = Int
  // expected-note@-1 {{move the type alias to another extension where it can be declared 'public' to satisfy the requirement}} {{none}}
  typealias AssocB = String
  // expected-note@-1 {{move the type alias to another extension where it can be declared 'public' to satisfy the requirement}} {{none}}
  func foo() {}
  // expected-note@-1 {{move the instance method to another extension where it can be declared 'public' to satisfy the requirement}} {{none}}
}

public struct Adopter4<T> : ProtoWithReqs {}
// expected-error@-1 {{method 'foo()' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
// expected-error@-2 {{type alias 'AssocA' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
// expected-error@-3 {{type alias 'AssocB' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
package extension Adopter4 {
  internal typealias AssocA = Int
  // expected-note@-1 {{move the type alias to another extension where it can be declared 'public' to satisfy the requirement}} {{none}}
  typealias AssocB = String
  // expected-note@-1 {{move the type alias to another extension where it can be declared 'public' to satisfy the requirement}} {{none}}
  fileprivate func foo() {}
  // expected-note@-1 {{move the instance method to another extension where it can be declared 'public' to satisfy the requirement}} {{none}}
}


package protocol PkgProtoWithReqs {
  associatedtype AssocA // expected-note * {{type declared here}}
  associatedtype AssocB // expected-note * {{type declared here}}
  func foo()
}

package struct Adopter5<T> : PkgProtoWithReqs {}
// expected-error@-1 {{method 'foo()' must be declared package because it matches a requirement in package protocol 'PkgProtoWithReqs'}} {{none}}
// expected-error@-2 {{type alias 'AssocA' must be declared package because it matches a requirement in package protocol 'PkgProtoWithReqs'}} {{none}}
// expected-error@-3 {{type alias 'AssocB' must be declared package because it matches a requirement in package protocol 'PkgProtoWithReqs'}} {{none}}
extension Adopter5 {
  typealias AssocA = Int
  // expected-note@-1 {{mark the type alias as 'package' to satisfy the requirement}} {{3-3=package }}
  typealias AssocB = String
  // expected-note@-1 {{mark the type alias as 'package' to satisfy the requirement}} {{3-3=package }}
  func foo() {}
  // expected-note@-1 {{mark the instance method as 'package' to satisfy the requirement}} {{3-3=package }}
}

public class AnotherAdopterBase {
  typealias AssocA = Int
  // expected-note@-1 {{mark the type alias as 'public' to satisfy the requirement}} {{3-3=public }}
  package typealias AssocB = String
  // expected-note@-1 {{mark the type alias as 'public' to satisfy the requirement}} {{3-10=public}}
  func foo() {}
  // expected-note@-1 {{mark the instance method as 'public' to satisfy the requirement}} {{3-3=public }}
}
public class AnotherAdopterSub : AnotherAdopterBase, ProtoWithReqs {}
// expected-error@-1 {{method 'foo()' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
// expected-error@-2 {{type alias 'AssocA' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
// expected-error@-3 {{type alias 'AssocB' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}

public class AnotherAdopterBase2 {}
public extension AnotherAdopterBase2 {
  internal typealias AssocA = Int
  // expected-note@-1 {{mark the type alias as 'public' to satisfy the requirement}} {{3-12=}}
  package typealias AssocB = String
  // expected-note@-1 {{mark the type alias as 'public' to satisfy the requirement}} {{3-11=}}
  fileprivate func foo() {}
  // expected-note@-1 {{mark the instance method as 'public' to satisfy the requirement}} {{3-15=}}
}
public class AnotherAdopterSub2 : AnotherAdopterBase2, ProtoWithReqs {}
// expected-error@-1 {{method 'foo()' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
// expected-error@-2 {{type alias 'AssocA' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
// expected-error@-3 {{type alias 'AssocB' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}

public class AnotherAdopterBase3 {}
internal extension AnotherAdopterBase3 {
  typealias AssocA = Int
  // expected-note@-1 {{move the type alias to another extension where it can be declared 'public' to satisfy the requirement}} {{none}}
  typealias AssocB = String
  // expected-note@-1 {{move the type alias to another extension where it can be declared 'public' to satisfy the requirement}} {{none}}
  func foo() {}
  // expected-note@-1 {{move the instance method to another extension where it can be declared 'public' to satisfy the requirement}} {{none}}
}
public class AnotherAdopterSub3 : AnotherAdopterBase3, ProtoWithReqs {}
// expected-error@-1 {{method 'foo()' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
// expected-error@-2 {{type alias 'AssocA' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
// expected-error@-3 {{type alias 'AssocB' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}

public class AnotherAdopterBase4 {}
package extension AnotherAdopterBase4 {
  typealias AssocA = Int
  // expected-note@-1 {{move the type alias to another extension where it can be declared 'public' to satisfy the requirement}} {{none}}
  typealias AssocB = String
  // expected-note@-1 {{move the type alias to another extension where it can be declared 'public' to satisfy the requirement}} {{none}}
  func foo() {}
  // expected-note@-1 {{move the instance method to another extension where it can be declared 'public' to satisfy the requirement}} {{none}}
}
public class AnotherAdopterSub4 : AnotherAdopterBase4, ProtoWithReqs {}
// expected-error@-1 {{method 'foo()' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
// expected-error@-2 {{type alias 'AssocA' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
// expected-error@-3 {{type alias 'AssocB' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}

package class AnotherAdopterBase5 {}
internal extension AnotherAdopterBase5 {
  typealias AssocA = Int
  // expected-note@-1 {{move the type alias to another extension where it can be declared 'package' to satisfy the requirement}} {{none}}
  typealias AssocB = String
  // expected-note@-1 {{move the type alias to another extension where it can be declared 'package' to satisfy the requirement}} {{none}}
  func foo() {}
  // expected-note@-1 {{move the instance method to another extension where it can be declared 'package' to satisfy the requirement}} {{none}}
}
package class AnotherAdopterSub5 : AnotherAdopterBase5, PkgProtoWithReqs {}
// expected-error@-1 {{method 'foo()' must be declared package because it matches a requirement in package protocol 'PkgProtoWithReqs'}} {{none}}
// expected-error@-2 {{type alias 'AssocA' must be declared package because it matches a requirement in package protocol 'PkgProtoWithReqs'}} {{none}}
// expected-error@-3 {{type alias 'AssocB' must be declared package because it matches a requirement in package protocol 'PkgProtoWithReqs'}} {{none}}


public protocol ReqProvider {}
extension ReqProvider {
  func foo() {}
  // expected-note@-1 {{mark the instance method as 'public' to satisfy the requirement}} {{3-3=public }}
  package typealias AssocB = String
  // expected-note@-1 {{mark the type alias as 'public' to satisfy the requirement}} {{3-10=public}}
}
public struct AdoptViaProtocol : ProtoWithReqs, ReqProvider {
  // expected-error@-1 {{method 'foo()' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
  // expected-error@-2 {{type alias 'AssocB' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
  public typealias AssocA = Int
}

internal protocol InternalProtoWithReqs {
  associatedtype AssocA // expected-note * {{type declared here}}
  associatedtype AssocB // expected-note * {{type declared here}}
  func foo()
}

internal protocol InternalReqProvider {}
extension InternalReqProvider {
  fileprivate func foo() {}
  // expected-note@-1 {{mark the instance method as 'internal' to satisfy the requirement}} {{3-14=internal}}
  fileprivate typealias AssocB = String
  // expected-note@-1 {{mark the type alias as 'internal' to satisfy the requirement}} {{3-14=internal}}
}
internal struct InternalAdoptViaProtocol : InternalProtoWithReqs, InternalReqProvider {
  // expected-error@-1 {{method 'foo()' must be declared internal because it matches a requirement in internal protocol 'InternalProtoWithReqs'}} {{none}}
  // expected-error@-2 {{type alias 'AssocB' must be declared internal because it matches a requirement in internal protocol 'InternalProtoWithReqs'}} {{none}}
  internal typealias AssocA = Int
}

public protocol ReqProvider2 {}
extension ProtoWithReqs where Self : ReqProvider2 {
  func foo() {}
  // expected-note@-1 {{mark the instance method as 'public' to satisfy the requirement}} {{3-3=public }}
  typealias AssocB = String
  // expected-note@-1 {{mark the type alias as 'public' to satisfy the requirement}} {{3-3=public }}
}
public struct AdoptViaCombinedProtocol : ProtoWithReqs, ReqProvider2 {
  // expected-error@-1 {{method 'foo()' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
  // expected-error@-2 {{type alias 'AssocB' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
  public typealias AssocA = Int
}

public protocol PublicInitProto {
  var value: Int { get }
  init(value: Int)
}
public struct NonPublicInitStruct: PublicInitProto {
  public var value: Int
  init(value: Int) {
  // expected-error@-1 {{initializer 'init(value:)' must be declared public because it matches a requirement in public protocol 'PublicInitProto'}}
  // expected-note@-2 {{mark the initializer as 'public' to satisfy the requirement}}
    self.value = value
  }
}
public struct NonPublicMemberwiseInitStruct: PublicInitProto {
// expected-error@-1 {{initializer 'init(value:)' must be declared public because it matches a requirement in public protocol 'PublicInitProto'}}
  public var value: Int
}

package protocol PackageInitProto {
  var value: Int { get }
  init(value: Int)
}
package struct NonPackageInitStruct: PackageInitProto {
  package var value: Int
  init(value: Int) {
  // expected-error@-1 {{initializer 'init(value:)' must be declared package because it matches a requirement in package protocol 'PackageInitProto'}}
  // expected-note@-2 {{mark the initializer as 'package' to satisfy the requirement}}
    self.value = value
  }
}
package struct NonPackageMemberwiseInitStruct: PackageInitProto {
// expected-error@-1 {{initializer 'init(value:)' must be declared package because it matches a requirement in package protocol 'PackageInitProto'}}
  public var value: Int
}

// https://github.com/apple/swift/issues/57595

public protocol PublicEmptyInit {
  init()
}
public struct Buggy: PublicEmptyInit { 
  // expected-error@-1 {{initializer 'init()' must be declared public because it matches a requirement in public protocol 'PublicEmptyInit'}}
}

package protocol PkgEmptyInit {
  init()
}
public struct PkgBuggy: PkgEmptyInit {
  // expected-error@-1 {{initializer 'init()' must be declared package because it matches a requirement in package protocol 'PkgEmptyInit'}}
}

package protocol PkgReqProvider {}
extension PkgReqProvider {
  fileprivate func foo() {}
  // expected-note@-1 {{mark the instance method as 'package' to satisfy the requirement}} {{3-14=package}}
  typealias AssocB = String
  // expected-note@-1 {{mark the type alias as 'package' to satisfy the requirement}} {{3-3=package }}
}
package struct PkgAdoptViaProtocol : PkgProtoWithReqs, PkgReqProvider {
  // expected-error@-1 {{method 'foo()' must be declared package because it matches a requirement in package protocol 'PkgProtoWithReqs'}} {{none}}
  // expected-error@-2 {{type alias 'AssocB' must be declared package because it matches a requirement in package protocol 'PkgProtoWithReqs'}} {{none}}
  package typealias AssocA = Int
}

package protocol PkgReqProvider2 {}
extension PkgProtoWithReqs where Self : PkgReqProvider2 {
  func foo() {}
  // expected-note@-1 {{mark the instance method as 'package' to satisfy the requirement}} {{3-3=package }}
  typealias AssocB = String
  // expected-note@-1 {{mark the type alias as 'package' to satisfy the requirement}} {{3-3=package }}
}
package struct PkgAdoptViaCombinedProtocol : PkgProtoWithReqs, PkgReqProvider2 {
  // expected-error@-1 {{method 'foo()' must be declared package because it matches a requirement in package protocol 'PkgProtoWithReqs'}} {{none}}
  // expected-error@-2 {{type alias 'AssocB' must be declared package because it matches a requirement in package protocol 'PkgProtoWithReqs'}} {{none}}
  public typealias AssocA = Int
}
