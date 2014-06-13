// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -parse -verify -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 %s

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
