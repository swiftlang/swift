// RUN: %target-swift-frontend -typecheck -verify %s -I %S/Inputs \
// RUN:   -cxx-interoperability-mode=default -disable-availability-checking

// Foreign reference types are imported as classes, but unlike other imported
// classes they are plain pointers to a C or C++ record, so they are
// representable in C.

import CDeclFRT

@c(takesImmortal) func takesImmortal(_ x: Immortal) { }
@c(takesImmortalOptional) func takesImmortalOptional(_ x: Immortal?) { }
@c(returnsImmortal) func returnsImmortal() -> Immortal? { nil }
@c(takesShared) func takesShared(_ x: Shared) { }
@c(returnsShared) func returnsShared() -> Shared? { nil }

// A foreign reference type that is only ever forward-declared is no different:
// it is still a pointer to the C record.
@c(takesOpaque) func takesOpaque(_ x: Opaque) { }
@c(takesOpaqueOptional) func takesOpaqueOptional(_ x: Opaque?) { }
@c(returnsOpaque) func returnsOpaque() -> Opaque? { nil }

// 'Unmanaged' is only available for types that are compatible with
// 'AnyObject', which foreign reference types are not: they use their own
// retain/release operations rather than Swift's.
@c(unmanagedShared) func unmanagedShared(_ x: Unmanaged<Shared>) { }
// expected-error@-1 {{'Unmanaged' requires that 'Shared' be a class type}}
// expected-note@-2 {{requirement specified as 'Instance' : 'AnyObject' [with Instance = Shared]}}
@c(unmanagedImmortal) func unmanagedImmortal() -> Unmanaged<Immortal> { fatalError() }
// expected-error@-1 {{'Unmanaged' requires that 'Immortal' be a class type}}
// expected-note@-2 {{requirement specified as 'Instance' : 'AnyObject' [with Instance = Immortal]}}

// A '@c' function cannot be the retain or release operation of a foreign
// reference type it takes as a parameter, because the entry point retains and
// releases that parameter and would therefore call itself.
@c(retainShared) func swiftRetainShared(_ x: Shared) { }
// expected-error@-1 {{@c function implementing the retain operation of foreign reference type 'Shared' will cause infinite recursion; use `Unmanaged` or a pointer type like `UnsafeMutableRawPointer`}}
@c(releaseShared) func swiftReleaseShared(_ x: Shared?) { }
// expected-error@-1 {{@c function implementing the release operation of foreign reference type 'Shared' will cause infinite recursion; use `Unmanaged` or a pointer type like `UnsafeMutableRawPointer`}}

// Taking the value as a raw pointer sidesteps the reference counting, so it is
// a workable way to write the operation.
@c(retainOpaque) func swiftRetainOpaque(_ x: UnsafeMutableRawPointer) { }

// Only the operations of a type that actually appears in the signature matter.
@c(retainOpaque) func retainOpaqueName(_ x: Shared) { }

// An immortal foreign reference type has no retain/release operations to
// collide with.
@c(retainShared) func retainSharedName(_ x: Immortal) { }

// '@c @implementation' can implement a C function whose signature mentions a
// foreign reference type.

@implementation @c
func CImplTakesImmortal(_ value: Immortal) { }

@implementation @c
func CImplReturnsImmortal() -> Immortal { fatalError() }

@implementation @c
func CImplGetSharedValue(_ s: Shared) -> Int32 { s.value }

@implementation @c
func CImplGetOpaqueValue(_ o: Opaque) -> Int32 { opaqueValue(o) }

// The same restriction applies to '@c @implementation', where taking the
// value as a pointer is not an option: the signature has to keep matching the
// C declaration.
@implementation @c
func retainOpaque(_ o: Opaque) { }
// expected-error@-1 {{@c function implementing the retain operation of foreign reference type 'Opaque' will cause infinite recursion; use `Unmanaged` or a pointer type like `UnsafeMutableRawPointer`}}

@implementation @c
func releaseOpaque(_ o: Opaque) { }
// expected-error@-1 {{@c function implementing the release operation of foreign reference type 'Opaque' will cause infinite recursion; use `Unmanaged` or a pointer type like `UnsafeMutableRawPointer`}}

// A reference-counted foreign reference type can be returned only where the C
// declaration returns it retained (+1): the Swift body always produces a
// retained value, which would leak against an unretained (+0) result.
@implementation @c
func CImplReturnsRetainedShared(_ s: Shared) -> Shared { s }

@implementation @c
func CImplReturnsUnretainedShared(_ s: Shared) -> Shared { s }
// expected-error@-1 {{global function 'CImplReturnsUnretainedShared' cannot implement C function 'CImplReturnsUnretainedShared' because it returns a foreign reference type unretained ('SWIFT_RETURNS_UNRETAINED'), which is not yet supported}}

@implementation @c
func CImplReturnsUnannotatedShared(_ s: Shared) -> Shared { s }
// expected-error@-1 {{global function 'CImplReturnsUnannotatedShared' cannot implement C function 'CImplReturnsUnannotatedShared' because it returns a foreign reference type without a 'SWIFT_RETURNS_RETAINED' annotation, which is not yet supported}}
