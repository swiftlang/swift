// A subclass that does not inherit its superclass's designated initializers
// must not expose them: they are vtable stubs that trap with "Use of
// unimplemented initializer".

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name SwiftMod -typecheck \
// RUN:   -clang-header-expose-decls=all-public -emit-clang-header-path %t/SwiftMod.h
// RUN: %FileCheck %s --input-file %t/SwiftMod.h \
// RUN:   --implicit-check-not='Derived init()' --implicit-check-not='Derived::init()' \
// RUN:   --implicit-check-not='Derived init(swift::Int y)' --implicit-check-not='Derived::init(swift::Int y)'
// RUN: %target-interop-build-clangxx -std=c++17 -fsyntax-only -x c++-header %t/SwiftMod.h

open class Base {
  public init() {}
  public init(y: Int) {}
}

// Declares its own designated initializer, so inherits none of Base's.
public final class Derived: Base {
  public init(x: Int) { super.init() }
}

// Declares no designated initializer, so inherits all of Base's.
public final class Inheriting: Base {}

// CHECK: static SWIFT_INLINE_THUNK Base init()
// CHECK: static SWIFT_INLINE_THUNK Derived init(swift::Int x)
// CHECK: static SWIFT_INLINE_THUNK Inheriting init()
// CHECK: static SWIFT_INLINE_THUNK Inheriting init(swift::Int y)
