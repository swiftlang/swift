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

// 'CFTypeRef' is imported as 'AnyObject', but it names the C type
// 'const void *' and so is representable in C.
@implementation @c
func CImplTakesCFTypeRef(_ obj: CFTypeRef?) { }

@implementation @c
func CImplReturnsRetainedCFTypeRef() -> CFTypeRef? { fatalError() }

// 'Unmanaged<CFTypeRef>' composes the two rules, which is what an unaudited
// 'CFTypeRef'-returning C function imports as.
@implementation @c
func CImplReturnsUnauditedCFTypeRef() -> Unmanaged<CFTypeRef>? { fatalError() }

// Writing the same type as 'AnyObject' does not name the C type, so it cannot
// be used to implement the same function.
@implementation @c(CImplTakesCFTypeRef)
// expected-error@-1 {{could not find imported function 'CImplTakesCFTypeRef' matching global function 'CImplTakesAnyObject'; make sure you import the module or header that declares it}}
func CImplTakesAnyObject(_ obj: AnyObject?) { }
// expected-error@-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
