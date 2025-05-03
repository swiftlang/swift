// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_LifetimeDependence

// RUN: %target-swift-ide-test -print-module -module-to-print=CountedByNoEscapeClang -plugin-path %swift-plugin-dir -I %S/Inputs -source-filename=x -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature LifetimeDependence | %FileCheck %s

// swift-ide-test doesn't currently typecheck the macro expansions, so run the compiler as well
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/CountedByNoEscape.swiftmodule -I %S/Inputs -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature LifetimeDependence %s

// Check that ClangImporter correctly infers and expands @_SwiftifyImport macros for functions with __counted_by __noescape parameters.

import CountedByNoEscapeClang

// CHECK:      @lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient public func complexExpr(_ len: Int32, _ offset: Int32, _ p: inout MutableSpan<Int32>)
// CHECK-NEXT: @lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient public func nonnull(_ p: inout MutableSpan<Int32>)
// CHECK-NEXT: @lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient public func nullUnspecified(_ p: inout MutableSpan<Int32>)
// CHECK-NEXT: @lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient public func nullable(_ p: inout MutableSpan<Int32>?)
// CHECK-NEXT: @lifetime(copy p)
// CHECK-NEXT: @lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient public func returnLifetimeBound(_ len1: Int32, _ p: inout MutableSpan<Int32>) -> MutableSpan<Int32>
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func returnPointer(_ len: Int32) -> UnsafeMutableBufferPointer<Int32>
// CHECK-NEXT: @lifetime(p1: copy p1)
// CHECK-NEXT: @lifetime(p2: copy p2)
// CHECK-NEXT: @_alwaysEmitIntoClient public func shared(_ len: Int32, _ p1: inout MutableSpan<Int32>, _ p2: inout MutableSpan<Int32>)
// CHECK-NEXT: @lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient public func simple(_ p: inout MutableSpan<Int32>)
// CHECK-NEXT: @lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient public func swiftAttr(_ p: inout MutableSpan<Int32>)

@lifetime(p: copy p)
@inlinable
public func callComplexExpr(_ p: inout MutableSpan<CInt>) {
  complexExpr(CInt(p.count), 1, &p)
}

@lifetime(p: copy p)
@inlinable
public func callNonnull(_ p: inout MutableSpan<CInt>) {
  nonnull(&p)
}

@lifetime(p: copy p)
@inlinable
public func callNullUnspecified(_ p: inout MutableSpan<CInt>) {
  nullUnspecified(&p)
}

@lifetime(p: copy p)
@inlinable
public func callNullable(_ p: inout MutableSpan<CInt>?) {
  nullable(&p)
}

@lifetime(p: copy p)
@inlinable
public func callReturnLifetimeBound(_ p: inout MutableSpan<CInt>) {
  let a: MutableSpan<CInt> = returnLifetimeBound(2, &p)
}

@inlinable
public func callReturnPointer() {
  let a: UnsafeMutableBufferPointer<CInt>? = returnPointer(4) // call wrapper
  let b: UnsafeMutablePointer<CInt>? = returnPointer(4) // call unsafe interop
}

@lifetime(p: copy p)
@lifetime(p2: copy p2)
@inlinable
public func callShared(_ p: inout MutableSpan<CInt>, _ p2: inout MutableSpan<CInt>) {
  shared(CInt(p.count), &p, &p2)
}

@lifetime(p: copy p)
@inlinable
public func callSimple(_ p: inout MutableSpan<CInt>) {
  simple(&p)
}

@lifetime(p: copy p)
@inlinable
public func callSwiftAttr(_ p: inout MutableSpan<CInt>) {
  swiftAttr(&p)
}
