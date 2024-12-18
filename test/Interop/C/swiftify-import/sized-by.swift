// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -disable-availability-checking -plugin-path %swift-plugin-dir -o %t/CountedBy.swiftmodule -I %S/Inputs -enable-experimental-feature SafeInteropWrappers %s

// Check that ClangImporter correctly infers and expands @_SwiftifyImport macros for functions with __sized_by parameters.

import SizedByClang

@inlinable
public func callSimple(_ p: UnsafeMutableRawBufferPointer) {
  simple(p)
}

// Check that macros from swift_attr work the same as inferred macros.
@inlinable
public func callSwiftAttr(_ p: UnsafeMutableRawBufferPointer) {
  swiftAttr(p)
}

@inlinable
public func callShared(_ len: CInt, _ p1: UnsafeMutableRawBufferPointer, _ p2: UnsafeMutableRawBufferPointer) {
  shared(len, p1, p2)
}

@inlinable
public func callComplexExpr(_ len: CInt, _ offset: CInt, _ p: UnsafeMutableRawBufferPointer) {
  complexExpr(len, offset, p)
}


@inlinable
public func callNullUnspecified(_ p: UnsafeMutableRawBufferPointer) {
  nullUnspecified(p)
}

@inlinable
public func callNonnull(_ p: UnsafeMutableRawBufferPointer) {
  nonnull(p)
}

@inlinable
public func callNullable(_ p: UnsafeMutableRawBufferPointer?) {
  nullable(p)
}

@inlinable
public func callOpaque(_ p: UnsafeRawBufferPointer) {
  opaque(p)
}


