// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s \
// RUN:   -enable-experimental-feature CDecl

// REQUIRES: swift_feature_CDecl
// REQUIRES: objc_interop

import Foundation

@objc
class ObjCClass: NSObject { }

@c("objcClassReturn") func objcClassReturn() -> ObjCClass { fatalError() }
// expected-error @-1 {{global function cannot be marked '@c' because its result type cannot be represented in C}}
// expected-note @-2 {{classes cannot be represented in C}}

@c("objcClassParams") func objcClassParams(a: ObjCClass, b: ObjCClass) { }
// expected-error @-1 {{global function cannot be marked '@c' because the type of the parameter 1 cannot be represented in C}}
// expected-error @-2 {{global function cannot be marked '@c' because the type of the parameter 2 cannot be represented in C}}
// expected-note @-3 2 {{classes cannot be represented in C}}

@objc
protocol ObjCProtocol {}

@c("objcProtocol") func objcProtocol(a: ObjCProtocol) { }
// expected-error @-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{protocols cannot be represented in C}}

@objc
enum ObjCEnum: Int { case A, B }
@c("objcEnumUseInCDecl") func objcEnumUseInCDecl(a: ObjCEnum) { }
// expected-error @-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{Swift enums not marked '@c' cannot be represented in C}}

/// Objective-C accepts @c enums.
@c("CEnum")
enum CEnum: Int { case A, B }
@_cdecl("cdeclEnumUseInObjc") func cdeclEnumUseInObjc(a: CEnum) { }

enum SwiftEnum { case A, B }
@_cdecl("swiftEnumUseInObjc") func swiftEnumUseInObjc(a: SwiftEnum) { }
// expected-error @-1 {{global function cannot be marked '@_cdecl' because the type of the parameter cannot be represented in Objective-C}}
// expected-note @-2 {{Swift enums not marked '@c' or '@objc' cannot be represented in Objective-C}}
