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

// '@c @implementation' can implement a C function whose signature mentions a
// foreign reference type.

@implementation @c
func CImplTakesImmortal(_ value: Immortal) { }

@implementation @c
func CImplReturnsImmortal() -> Immortal { fatalError() }

@implementation @c
func CImplGetSharedValue(_ s: Shared) -> Int32 { s.value }
