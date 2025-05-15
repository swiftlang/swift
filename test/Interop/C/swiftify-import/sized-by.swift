// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %target-swift-ide-test -print-module -module-to-print=SizedByClang -plugin-path %swift-plugin-dir -I %S/Inputs -source-filename=x -enable-experimental-feature SafeInteropWrappers | %FileCheck %s

// swift-ide-test doesn't currently typecheck the macro expansions, so run the compiler as well
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/SizedBy.swiftmodule -I %S/Inputs -enable-experimental-feature SafeInteropWrappers %s

// Check that ClangImporter correctly infers and expands @_SwiftifyImport macros for functions with __sized_by parameters.
import SizedByClang

// CHECK:      @_alwaysEmitIntoClient public func complexExpr(_ len: Int{{.*}}, _ offset: Int{{.*}}, _ p: UnsafeMutableRawBufferPointer)
// CHECK-NEXT: @_alwaysEmitIntoClient public func nonnull(_  p: UnsafeMutableRawBufferPointer)
// CHECK-NEXT: @_alwaysEmitIntoClient public func nullUnspecified(_  p: UnsafeMutableRawBufferPointer)
// CHECK-NEXT: @_alwaysEmitIntoClient public func nullable(_  p: UnsafeMutableRawBufferPointer?)
// CHECK-NEXT: @_alwaysEmitIntoClient public func opaque(_  p: UnsafeRawBufferPointer)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func returnPointer(_ len: Int{{.*}}) -> UnsafeMutableRawBufferPointer
// CHECK-NEXT: @_alwaysEmitIntoClient public func shared(_ len: Int{{.*}}, _ p1: UnsafeMutableRawBufferPointer, _ p2: UnsafeMutableRawBufferPointer)
// CHECK-NEXT: @_alwaysEmitIntoClient public func simple(_  p: UnsafeMutableRawBufferPointer)
// CHECK-NEXT: @_alwaysEmitIntoClient public func swiftAttr(_  p: UnsafeMutableRawBufferPointer)

@inlinable
public func callComplexExpr(_ p: UnsafeMutableRawBufferPointer) {
  complexExpr(CInt(p.count), 1, p)
}

@inlinable
public func callNonnull(_ p: UnsafeMutableRawBufferPointer) {
  nonnull(p)
}

@inlinable
public func callNullUnspecified(_ p: UnsafeMutableRawBufferPointer) {
  nullUnspecified(p)
}

@inlinable
public func callNullable(_ p: UnsafeMutableRawBufferPointer?) {
  nullable(p)
}

@inlinable
public func callOpaque(_ p: UnsafeRawBufferPointer) {
  opaque(p)
}

@inlinable
public func callReturnPointer() {
  let a: UnsafeMutableRawBufferPointer? = returnPointer(4) // call wrapper
  let b: UnsafeMutableRawPointer? = returnPointer(4) // call unsafe interop
}

@inlinable
public func callShared(_ p: UnsafeMutableRawBufferPointer, _ p2: UnsafeMutableRawBufferPointer) {
  shared(CInt(p.count), p, p2)
}

@inlinable
public func callSimple(_ p: UnsafeMutableRawBufferPointer) {
  simple(p)
}

@inlinable
public func callSwiftAttr(_ p: UnsafeMutableRawBufferPointer) {
  swiftAttr(p)
}
