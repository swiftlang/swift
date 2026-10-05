// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s \
// RUN:   -disable-objc-interop

// REQUIRES: objc_interop

import CoreFoundation

// 'CFTypeRef' does not depend on Objective-C interop. It is reference counted
// the way 'AnyObject' is, which means Objective-C reference counting where
// Objective-C interop is enabled and 'swift_retain'/'swift_release' where it is
// not; a CF type reached through 'CFTypeRef' is required to use whichever of
// those is in effect.
@c(cfTypeRefParam) func cfTypeRefParam(a: CFTypeRef) { }
@c(cfTypeRefReturn) func cfTypeRefReturn() -> CFTypeRef? { fatalError() }

// Spelling the same type as 'AnyObject' still does not name a C type.
@c(anyObjectParam) func anyObjectParam(a: AnyObject) { }
// expected-error@-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note@-2 {{protocols cannot be represented in C}}

// CF class types do not depend on Objective-C interop either.
@c(cfStringParam) func cfStringParam(a: CFString) { }
