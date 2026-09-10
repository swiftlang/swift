// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s \
// RUN:   -import-objc-header %S/Inputs/c_implementation_cf.h \
// RUN:   -target %target-stable-abi-triple

// REQUIRES: objc_interop

import CoreFoundation

// CF class types are representable in C, so they can be used in the signature
// of an '@c @implementation' function.
@implementation @c
func CImplTakesCFArray(_ arr: CFArray?) { }

@implementation @c
func CImplTakesCFTree(_ tree: CFTree?) { }

@implementation @c
func CImplTakesConsumedCFString(_ string: consuming CFString?) { }

@implementation @c
func CImplReturnsRetainedCFString() -> CFString? { fatalError() }

@implementation @c
func CImplReturnsNotRetainedCFString() -> CFString? { fatalError() }

// An unaudited CF-returning function is imported as returning
// 'Unmanaged<CFString>?', which is representable in C too.
@implementation @c
func CImplReturnsUnauditedCFString() -> Unmanaged<CFString>? { fatalError() }

// 'CFTypeRef' is imported as 'AnyObject', which is not representable in C, so
// there is no way to write an implementation for a function that takes one.
// FIXME: This should be allowed.
@implementation @c
func CImplTakesCFTypeRef(_ obj: CFTypeRef?) { }
// expected-error@-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-error@-2 {{selector 'CImplTakesCFTypeRef:' for global function 'CImplTakesCFTypeRef' not found in header; did you mean 'CImplTakesCFTypeRef'?}}
