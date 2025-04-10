// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_LifetimeDependence

// RUN: %target-swift-ide-test -print-module -module-to-print=CountedByNoEscapeClang -plugin-path %swift-plugin-dir -I %S/Inputs -source-filename=x -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature LifetimeDependence | %FileCheck %s

// swift-ide-test doesn't currently typecheck the macro expansions, so run the compiler as well
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/CountedByNoEscape.swiftmodule -I %S/Inputs -enable-experimental-feature SafeInteropWrappers %s

// Check that ClangImporter correctly infers and expands @_SwiftifyImport macros for functions with __counted_by __noescape parameters.

import CountedByNoEscapeClang

// CHECK:      @lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient public func complexExpr(_ len: Int32, _ offset: Int32, _ p: inout MutableSpan<Int32>)
// CHECK-NEXT: @lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient public func nonnull(_ p: inout MutableSpan<Int32>)
// CHECK-NEXT: @lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient public func nullUnspecified(_ p: inout MutableSpan<Int32>)
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

@inlinable
public func callReturnPointer() {
  let a: UnsafeMutableBufferPointer<CInt>? = returnPointer(4) // call wrapper
  let b: UnsafeMutablePointer<CInt>? = returnPointer(4) // call unsafe interop
}

