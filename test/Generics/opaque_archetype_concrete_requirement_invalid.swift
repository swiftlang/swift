// RUN: %target-swift-frontend -typecheck -verify %s -target %target-swift-5.1-abi-triple -enable-requirement-machine-opaque-archetypes

protocol P1 {}

struct S_P1 : P1 {}

protocol P {
  associatedtype T

  var t: T { get }
}

struct DefinesOpaqueP1 : P {
  var t: some P1 {
    return S_P1()
  }
}

protocol HasP {
  associatedtype T : P1
  associatedtype U
}

extension HasP where T == DefinesOpaqueP1.T, U == T.DoesNotExist {}
// expected-error@-1 {{'DoesNotExist' is not a member type of type 'Self.T'}}
