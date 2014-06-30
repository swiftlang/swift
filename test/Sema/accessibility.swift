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

@internal struct InternalStruct: PublicProto, InternalProto, PrivateProto {
  @private func publicReq() {} // expected-error {{method 'publicReq()' must be as accessible as its enclosing type because it matches a requirement in protocol 'PublicProto'}} {{3-11=@internal}}
  @private func internalReq() {} // expected-error {{method 'internalReq()' must have internal accessibility because it matches a requirement in internal protocol 'InternalProto'}} {{3-11=@internal}}
  @private func privateReq() {}
}

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
