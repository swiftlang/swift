// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s

// REQUIRES: objc_interop

import CoreFoundation

@c(cfStringReturn) func cfClassReturn() -> CFString { fatalError() }
@c(cfStringOptionalReturn) func cfClassOptionalReturn() -> CFString? { fatalError() }
@c(cfObjectParams) func cfClassParams(a: CFArray, b: CFDictionary) { }
@c(cfObjectOptionalParams) func cfClassParams(a: CFArray?, b: CFDictionary?) { }

@c(unmanagedCFStringReturn) func unmanagedCFClassReturn() -> Unmanaged<CFString> { fatalError() }
// expected-error@-1 {{global function cannot be marked '@c' because its result type cannot be represented in C}}
// expected-note@-2 {{Swift structs cannot be represented in C}}
@c(unmanagedCFObjectParams) func unmanagedCFClassParams(a: Unmanaged<CFArray>, b: Unmanaged<CFDictionary>) { }
// expected-error@-1 {{global function cannot be marked '@c' because the type of the parameter 1 cannot be represented in C}}
// expected-error@-2 {{global function cannot be marked '@c' because the type of the parameter 2 cannot be represented in C}}
// expected-note@-3 {{Swift structs cannot be represented in C}}
// expected-note@-4 {{Swift structs cannot be represented in C}}

@c(unmanagedOptioanlCFStringReturn) func unmanagedCFClassReturn() -> Unmanaged<CFString>? { fatalError() }
// expected-error@-1 {{global function cannot be marked '@c' because its result type cannot be represented in C}}
@c(unmanagedOptioanlCFObjectParams) func unmanagedCFClassParams(a: Unmanaged<CFArray>?, b: Unmanaged<CFDictionary>?) { }
// expected-error@-1 {{global function cannot be marked '@c' because the type of the parameter 1 cannot be represented in C}}
// expected-error@-2 {{global function cannot be marked '@c' because the type of the parameter 2 cannot be represented in C}}

