// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s \
// RUN:   -enable-experimental-feature CDecl

// REQUIRES: swift_feature_CDecl
// REQUIRES: objc_interop

import Foundation

@objc
class ObjCClass: NSObject { }

@cdecl("objcClassReturn") func objcClassReturn() -> ObjCClass { fatalError() }
// expected-error @-1 {{global function cannot be marked '@cdecl' because its result type cannot be represented in C}}
// expected-note @-2 {{classes cannot be represented in C}}

@cdecl("objcClassParams") func objcClassParams(a: ObjCClass, b: ObjCClass) { }
// expected-error @-1 {{global function cannot be marked '@cdecl' because the type of the parameter 1 cannot be represented in C}}
// expected-error @-2 {{global function cannot be marked '@cdecl' because the type of the parameter 2 cannot be represented in C}}
// expected-note @-3 2 {{classes cannot be represented in C}}

@objc
protocol ObjCProtocol {}

@cdecl("objcProtocol") func objcProtocol(a: ObjCProtocol) { }
// expected-error @-1 {{global function cannot be marked '@cdecl' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{protocols cannot be represented in C}}
