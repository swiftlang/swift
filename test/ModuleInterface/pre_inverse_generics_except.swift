// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Test.swiftinterface) %s \
// RUN:   -module-name Test \
// RUN:   -enable-experimental-feature PreInverseGenericsExcept

// RUN: %FileCheck --implicit-check-not '#if' %s < %t/Test.swiftinterface

// REQUIRES: swift_feature_PreInverseGenericsExcept

// The bare @_preInverseGenerics needs no feature guard.
// CHECK:      @_preInverseGenerics public func bare<T>(_ t: borrowing T) where T : ~Copyable
@_preInverseGenerics
public func bare<T: ~Copyable>(_ t: borrowing T) {}

// The except: form requires a #if $PreInverseGenericsExcept guard.
// Older compilers that don't support the feature will not see the declaration.

// CHECK:      #if compiler(>=5.3) && $PreInverseGenericsExcept
// CHECK-NEXT: @_preInverseGenerics(except: ~Copyable) public func exceptCopyable<T>(_ t: borrowing T) where T : ~Copyable, T : ~Escapable
// CHECK-NEXT: #endif
@_preInverseGenerics(except: ~Copyable)
public func exceptCopyable<T: ~Copyable & ~Escapable>(_ t: borrowing T) {}

// CHECK:      #if compiler(>=5.3) && $PreInverseGenericsExcept
// CHECK-NEXT: @_preInverseGenerics(except: ~Escapable) public func exceptEscapable<T>(_ t: borrowing T) where T : ~Copyable, T : ~Escapable
// CHECK-NEXT: #endif
@_preInverseGenerics(except: ~Escapable)
public func exceptEscapable<T: ~Copyable & ~Escapable>(_ t: borrowing T) {}

// CHECK:      #if compiler(>=5.3) && $PreInverseGenericsExcept
// CHECK-NEXT: @_preInverseGenerics(except: ~Copyable & ~Escapable) public func exceptBoth<T>(_ t: borrowing T) where T : ~Copyable, T : ~Escapable
@_preInverseGenerics(except: ~Copyable & ~Escapable)
public func exceptBoth<T: ~Copyable & ~Escapable>(_ t: borrowing T) {}


@frozen
public struct MySpan<T: ~Copyable & ~Escapable>: ~Copyable {
// CHECK:      #if compiler(>=5.3) && $PreInverseGenericsExcept
// CHECK-NEXT: @_preInverseGenerics(except: ~Copyable) public var _count: Swift::Int
// CHECK-NEXT: #endif
  @_preInverseGenerics(except: ~Copyable)
  public var _count: Int
}
