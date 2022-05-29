// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s -disable-objc-attr-requires-foundation-module

// REQUIRES: objc_interop

import ObjectiveC

@objc protocol ObjCProto {}
@objc protocol ObjCProto2 {}
protocol NonObjCProto {}
typealias TwoObjCProtos = ObjCProto & ObjCProto2

func takesProtocol(_ x: Protocol) {}

takesProtocol(ObjCProto.self)
takesProtocol(ObjCProto2.self)
takesProtocol(NonObjCProto.self) // expected-error{{cannot convert value of type '(any NonObjCProto).Type' to expected argument type 'Protocol'}}
takesProtocol(TwoObjCProtos.self) // expected-error{{cannot convert value of type '(any TwoObjCProtos).Type' (aka '(any ObjCProto & ObjCProto2).Type') to expected argument type 'Protocol'}}
