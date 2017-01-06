// RUN: %target-typecheck-verify-swift -swift-version 3

public struct Pair<A, B> {}

public struct PublicStruct {
  public struct Inner {}
  internal struct Internal {}
}

private typealias PrivateAlias = PublicStruct // expected-note {{type declared here}}

public let a: PrivateAlias.Inner?
public let b: PrivateAlias.Internal? // expected-error {{constant cannot be declared public because its type uses an internal type}}
public let c: Pair<PrivateAlias.Inner, PublicStruct.Internal>? // expected-error {{constant cannot be declared public because its type uses an internal type}}
public let c2: Pair<PublicStruct.Internal, PrivateAlias.Inner>? // expected-error {{constant cannot be declared public because its type uses an internal type}}
public let d: PrivateAlias? // expected-error {{constant cannot be declared public because its type uses a private type}}


// rdar://problem/21408035
private class PrivateBox<T> { // expected-note {{type declared here}}
  typealias ValueType = T
  typealias AlwaysFloat = Float
}

let boxUnboxInt: PrivateBox<Int>.ValueType = 0 // FIXME: This used to error in Swift 3.0.
let boxFloat: PrivateBox<Int>.AlwaysFloat = 0 // expected-error {{constant must be declared private or fileprivate because its type uses a private type}}

private protocol PrivateProto {
  associatedtype Inner
}
extension PublicStruct: PrivateProto {}

private class SpecificBox<T: PrivateProto> { // expected-note {{type declared here}}
  typealias InnerType = T.Inner
  typealias AlwaysFloat = Float
}

let specificBoxUnboxInt: SpecificBox<PublicStruct>.InnerType = .init() // FIXME: This used to error in Swift 3.0.
let specificBoxFloat: SpecificBox<PublicStruct>.AlwaysFloat = 0 // expected-error {{constant must be declared private or fileprivate because its type uses a private type}}

