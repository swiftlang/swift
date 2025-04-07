// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s \
// RUN:   -enable-experimental-feature CDecl

// REQUIRES: swift_feature_CDecl
// REQUIRES: objc_interop

import Foundation

@objc
class MyClass: NSObject { }

@cdecl("cdecl_c") func objc_ref() -> MyClass { fatalError() }
// expected-error @-1 {{global function cannot be marked @cdecl because its result type cannot be represented in C}}

@cdecl("cdecl_c2") func objc_ref(a: MyClass, b: MyClass) { }
// expected-error @-1 {{global function cannot be marked @cdecl because the type of the parameter 1 cannot be represented in C}}
// expected-error @-2 {{global function cannot be marked @cdecl because the type of the parameter 2 cannot be represented in C}}
