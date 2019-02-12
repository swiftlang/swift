// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck -disable-access-control %s

public protocol ProtoWithReqs {
  associatedtype Assoc
  func foo()
}

public struct Adopter<T> : ProtoWithReqs {}
// expected-error@-1 {{method 'foo()' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
// expected-error@-2 {{type alias 'Assoc' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
extension Adopter {
  typealias Assoc = Int
  // expected-note@-1 {{mark the type alias as 'public' to satisfy the requirement}} {{3-3=public }}
  func foo() {}
  // expected-note@-1 {{mark the instance method as 'public' to satisfy the requirement}} {{3-3=public }}
}

public struct Adopter2<T> : ProtoWithReqs {}
// expected-error@-1 {{method 'foo()' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
// expected-error@-2 {{type alias 'Assoc' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
public extension Adopter2 {
  internal typealias Assoc = Int
  // expected-note@-1 {{mark the type alias as 'public' to satisfy the requirement}} {{3-12=}}
  fileprivate func foo() {}
  // expected-note@-1 {{mark the instance method as 'public' to satisfy the requirement}} {{3-15=}}
}

public struct Adopter3<T> : ProtoWithReqs {}
// expected-error@-1 {{method 'foo()' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
// expected-error@-2 {{type alias 'Assoc' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
internal extension Adopter3 {
  typealias Assoc = Int
  // expected-note@-1 {{move the type alias to another extension where it can be declared 'public' to satisfy the requirement}} {{none}}
  func foo() {}
  // expected-note@-1 {{move the instance method to another extension where it can be declared 'public' to satisfy the requirement}} {{none}}
}

public class AnotherAdopterBase {
  typealias Assoc = Int
  // expected-note@-1 {{mark the type alias as 'public' to satisfy the requirement}} {{3-3=public }}
  func foo() {}
  // expected-note@-1 {{mark the instance method as 'public' to satisfy the requirement}} {{3-3=public }}
}
public class AnotherAdopterSub : AnotherAdopterBase, ProtoWithReqs {}
// expected-error@-1 {{method 'foo()' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
// expected-error@-2 {{type alias 'Assoc' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}

public class AnotherAdopterBase2 {}
public extension AnotherAdopterBase2 {
  internal typealias Assoc = Int
  // expected-note@-1 {{mark the type alias as 'public' to satisfy the requirement}} {{3-12=}}
  fileprivate func foo() {}
  // expected-note@-1 {{mark the instance method as 'public' to satisfy the requirement}} {{3-15=}}
}
public class AnotherAdopterSub2 : AnotherAdopterBase2, ProtoWithReqs {}
// expected-error@-1 {{method 'foo()' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
// expected-error@-2 {{type alias 'Assoc' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}

public class AnotherAdopterBase3 {}
internal extension AnotherAdopterBase3 {
  typealias Assoc = Int
  // expected-note@-1 {{move the type alias to another extension where it can be declared 'public' to satisfy the requirement}} {{none}}
  func foo() {}
  // expected-note@-1 {{move the instance method to another extension where it can be declared 'public' to satisfy the requirement}} {{none}}
}
public class AnotherAdopterSub3 : AnotherAdopterBase3, ProtoWithReqs {}
// expected-error@-1 {{method 'foo()' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
// expected-error@-2 {{type alias 'Assoc' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}

public protocol ReqProvider {}
extension ReqProvider {
  func foo() {}
  // expected-note@-1 {{mark the instance method as 'public' to satisfy the requirement}} {{3-3=public }}
}
public struct AdoptViaProtocol : ProtoWithReqs, ReqProvider {
  // expected-error@-1 {{method 'foo()' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
  public typealias Assoc = Int
}

public protocol ReqProvider2 {}
extension ProtoWithReqs where Self : ReqProvider2 {
  func foo() {}
  // expected-note@-1 {{mark the instance method as 'public' to satisfy the requirement}} {{3-3=public }}
}
public struct AdoptViaCombinedProtocol : ProtoWithReqs, ReqProvider2 {
  // expected-error@-1 {{method 'foo()' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
  public typealias Assoc = Int
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
