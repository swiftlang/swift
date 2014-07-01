// RUN: %swift %s -parse -verify

@public protocol PublicProto {
  func publicReq()
}

@internal protocol InternalProto {
  func internalReq()
}

@private protocol PrivateProto {
  func privateReq()
}

@public struct PublicStruct: PublicProto, InternalProto, PrivateProto {
  @private func publicReq() {} // expected-error {{method 'publicReq()' must have public accessibility because it matches a requirement in public protocol 'PublicProto'}} {{3-11=@public}}
  @private func internalReq() {} // expected-error {{method 'internalReq()' must have internal accessibility because it matches a requirement in internal protocol 'InternalProto'}} {{3-11=@internal}}
  @private func privateReq() {}
}

// expected-note@+1 {{type declared here}}
@internal struct InternalStruct: PublicProto, InternalProto, PrivateProto {
  @private func publicReq() {} // expected-error {{method 'publicReq()' must be as accessible as its enclosing type because it matches a requirement in protocol 'PublicProto'}} {{3-11=@internal}}
  @private func internalReq() {} // expected-error {{method 'internalReq()' must have internal accessibility because it matches a requirement in internal protocol 'InternalProto'}} {{3-11=@internal}}
  @private func privateReq() {}
}

// expected-note@+1 17 {{type declared here}}
@private struct PrivateStruct: PublicProto, InternalProto, PrivateProto {
  @private func publicReq() {}
  @private func internalReq() {}
  @private func privateReq() {}
}

@public struct PublicStructDefaultMethods: PublicProto, InternalProto, PrivateProto {
  func publicReq() {} // expected-error {{method 'publicReq()' must have public accessibility because it matches a requirement in public protocol 'PublicProto'}} {{3-3=@public }}
  func internalReq() {}
  func privateReq() {}
}


@public class Base {
  @public @required init() {}
}

@public class PublicSub: Base {
  init() {} // expected-error {{'required' initializer must be as accessible as its enclosing type}} {{3-3=@public }}
}

@internal class InternalSub: Base {
  @private init() {} // expected-error {{'required' initializer must be as accessible as its enclosing type}} {{3-11=@internal}}
}

@internal class InternalSubGood: Base {
  init() {} // no-warning
}


@public typealias PublicTA1 = PublicStruct
@public typealias PublicTA2 = InternalStruct // expected-error {{type alias cannot be declared public because its underlying type uses an internal type}}
@public typealias PublicTA3 = PrivateStruct // expected-error {{type alias cannot be declared public because its underlying type uses a private type}}

@internal typealias InternalTA1 = PublicStruct
@internal typealias InternalTA2 = InternalStruct
@internal typealias InternalTA3 = PrivateStruct // expected-error {{type alias cannot be declared internal because its underlying type uses a private type}}

typealias FunctionType1 = PrivateStruct -> PublicStruct // expected-error {{type alias must be declared private because its underlying type uses a private type}}
typealias FunctionType2 = PublicStruct -> PrivateStruct // expected-error {{type alias must be declared private because its underlying type uses a private type}}
typealias FunctionType3 = PrivateStruct -> PrivateStruct // expected-error {{type alias must be declared private because its underlying type uses a private type}}

typealias ArrayType = [PrivateStruct] // expected-error {{type alias must be declared private because its underlying type uses a private type}}
typealias DictType = [String : PrivateStruct] // expected-error {{type alias must be declared private because its underlying type uses a private type}}
typealias GenericArgs = Optional<PrivateStruct> // expected-error {{type alias must be declared private because its underlying type uses a private type}}


@public protocol HasAssocType {
  typealias Inferred
  func test(input: Inferred)
}

@public struct AssocTypeImpl: HasAssocType {
  @public func test(input: Bool) {}
}
@public let _: AssocTypeImpl.Inferred?


@public let x: PrivateStruct = PrivateStruct() // expected-error {{constant cannot be declared public because its type uses a private type}}
@public var a: PrivateStruct?, b: PrivateStruct? // expected-error 2 {{variable cannot be declared public because its type uses a private type}}
@public var (c, d): (PrivateStruct?, PrivateStruct?) // expected-error {{variable cannot be declared public because its type uses a private type}}

var internalVar: PrivateStruct? // expected-error {{variable must be declared private because its type uses a private type}}

@public struct Properties {
  @public let x: PrivateStruct = PrivateStruct() // expected-error {{property cannot be declared public because its type uses a private type}}
  @public var a: PrivateStruct?, b: PrivateStruct? // expected-error 2 {{property cannot be declared public because its type uses a private type}}
  @public var (c, d): (PrivateStruct?, PrivateStruct?) // expected-error {{property cannot be declared public because its type uses a private type}}
}
