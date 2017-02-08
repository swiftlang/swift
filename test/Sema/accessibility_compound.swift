// RUN: %target-typecheck-verify-swift -swift-version 4

public struct Pair<A, B> {}

public struct PublicStruct {
  public struct Inner {}
  internal struct Internal {}
}

private typealias PrivateAlias = PublicStruct // expected-note * {{type declared here}}

public let a0 = nil as PrivateAlias.Inner? // expected-error {{constant cannot be declared public because its type 'PrivateAlias.Inner?' uses a private type}}
public let a: PrivateAlias.Inner? // expected-error {{constant cannot be declared public because its type uses a private type}}
public let b: PrivateAlias.Internal? // expected-error {{constant cannot be declared public because its type uses a private type}}
public let c: Pair<PrivateAlias.Inner, PublicStruct.Internal>? // expected-error {{constant cannot be declared public because its type uses a private type}}
public let c2: Pair<PublicStruct.Internal, PrivateAlias.Inner>? // expected-error {{constant cannot be declared public because its type uses a private type}}
public let d: PrivateAlias? // expected-error {{constant cannot be declared public because its type uses a private type}}


// rdar://problem/21408035
private class PrivateBox<T> { // expected-note 2 {{type declared here}}
  typealias ValueType = T
  typealias AlwaysFloat = Float
}

let boxUnboxInt: PrivateBox<Int>.ValueType = 0 // expected-error {{constant must be declared private or fileprivate because its type uses a private type}}
let boxFloat: PrivateBox<Int>.AlwaysFloat = 0 // expected-error {{constant must be declared private or fileprivate because its type uses a private type}}

private protocol PrivateProto {
  associatedtype Inner
}
extension PublicStruct: PrivateProto {}

private class SpecificBox<T: PrivateProto> { // expected-note 2 {{type declared here}}
  typealias InnerType = T.Inner
  typealias AlwaysFloat = Float
}

let specificBoxUnboxInt: SpecificBox<PublicStruct>.InnerType = .init() // expected-error {{constant must be declared private or fileprivate because its type uses a private type}}
let specificBoxFloat: SpecificBox<PublicStruct>.AlwaysFloat = 0 // expected-error {{constant must be declared private or fileprivate because its type uses a private type}}

