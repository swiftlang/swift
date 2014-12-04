// RUN: %swift %clang-importer-sdk -parse -verify -target x86_64-apple-macosx10.9 %s

import ObjectiveC

@objc protocol ObjCProto {}
@objc protocol ObjCProto2 {}
protocol NonObjCProto {}
typealias TwoObjCProtos = protocol<ObjCProto, ObjCProto2>

func takesProtocol(x: Protocol) {} // expected-note{{}} expected-note{{}}

takesProtocol(ObjCProto.self)
takesProtocol(ObjCProto2.self)
takesProtocol(NonObjCProto.self) // expected-error{{'NonObjCProto.Protocol' is not convertible to 'Protocol'}}
takesProtocol(TwoObjCProtos.self) // expected-error{{'TwoObjCProtos.Protocol' is not convertible to 'Protocol'}}
