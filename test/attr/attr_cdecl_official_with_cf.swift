// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s

// REQUIRES: objc_interop

import CoreFoundation

@c(cfStringReturn) func cfClassReturn() -> CFString { fatalError() }
@c(cfStringOptionalReturn) func cfClassOptionalReturn() -> CFString? { fatalError() }
@c(cfObjectParams) func cfClassParams(a: CFArray, b: CFDictionary) { }
@c(cfObjectOptionalParams) func cfClassParams(a: CFArray?, b: CFDictionary?) { }

// Unmanaged<T> is representable wherever T is, so an unmanaged CF type is
// representable in C.
@c(unmanagedCFStringReturn) func unmanagedCFClassReturn() -> Unmanaged<CFString> { fatalError() }
@c(unmanagedCFObjectParams) func unmanagedCFClassParams(a: Unmanaged<CFArray>, b: Unmanaged<CFDictionary>) { }

@c(unmanagedOptioanlCFStringReturn) func unmanagedCFClassReturn() -> Unmanaged<CFString>? { fatalError() }
@c(unmanagedOptioanlCFObjectParams) func unmanagedCFClassParams(a: Unmanaged<CFArray>?, b: Unmanaged<CFDictionary>?) { }

// ...but the object type still has to be representable in C.
class SwiftClass {}
@c(unmanagedSwiftClassParam) func unmanagedSwiftClassParam(a: Unmanaged<SwiftClass>) { }
// expected-error@-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note@-2 {{Swift structs cannot be represented in C}}

// CF types that are not bridged to an Objective-C class are representable too.
@c(cfTreeParams) func cfTreeParams(a: CFTree, b: CFTree?) { }
@c(cfAllocatorReturn) func cfAllocatorReturn() -> CFAllocator? { fatalError() }

// 'CFTypeRef' is imported as 'AnyObject', but it names the C type
// 'const void *', so it is representable in C. A value passed this way is
// reference counted the way 'AnyObject' is.
@c(cfTypeRefParam) func cfTypeRefParam(a: CFTypeRef) { }
@c(cfTypeRefOptionalParam) func cfTypeRefOptionalParam(a: CFTypeRef?) { }
@c(cfTypeRefReturn) func cfTypeRefReturn() -> CFTypeRef { fatalError() }
@c(cfTypeRefOptionalReturn) func cfTypeRefOptionalReturn() -> CFTypeRef? { fatalError() }

// A typealias of 'CFTypeRef' names the same C type, whether it is written in
// Swift or imported from C ('typedef CFTypeRef CFAliasForTypeRef').
typealias MyTypeRef = CFTypeRef
@c(myTypeRefParam) func myTypeRefParam(a: MyTypeRef) { }
@c(cfAliasForTypeParam) func cfAliasForTypeParam(a: CFAliasForType) { }

// Spelling the same type as 'AnyObject' does not name a C type, so it stays
// unrepresentable.
@c(anyObjectParam) func anyObjectParam(a: AnyObject) { }
// expected-error@-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note@-2 {{protocols cannot be represented in C}}
@c(anyObjectReturn) func anyObjectReturn() -> AnyObject? { fatalError() }
// expected-error@-1 {{global function cannot be marked '@c' because its result type cannot be represented in C}}

// Neither does 'Any'.
@c(anyParam) func anyParam(a: Any) { }
// expected-error@-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note@-2 {{protocols cannot be represented in C}}
