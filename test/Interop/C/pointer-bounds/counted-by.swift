// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: pointer_bounds

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -disable-availability-checking -plugin-path %swift-plugin-dir -o %t/CountedBy.swiftmodule -I %S/Inputs -enable-experimental-feature SafeInteropWrappers %s

// Check that ClangImporter correctly infers and expands @PointerBounds macros for functions with __counted_by parameters.

import CountedByClang

@inlinable
public func callSimple(_ p: UnsafeMutableBufferPointer<CInt>) {
  simple(p)
}

// Check that macros from swift_attr work the same as inferred macros.
@inlinable
public func callSwiftAttr(_ p: UnsafeMutableBufferPointer<CInt>) {
  swiftAttr(p)
}

@inlinable
public func callShared(_ len: CInt, _ p1: UnsafeMutableBufferPointer<CInt>, _ p2: UnsafeMutableBufferPointer<CInt>) {
  shared(len, p1, p2)
}

@inlinable
public func callComplexExpr(_ len: CInt, _ offset: CInt, _ p: UnsafeMutableBufferPointer<CInt>) {
  complexExpr(len, offset, p)
}


@inlinable
public func callNullUnspecified(_ p: UnsafeMutableBufferPointer<CInt>) {
  nullUnspecified(p)
}

@inlinable
public func callNonnull(_ p: UnsafeMutableBufferPointer<CInt>) {
  nonnull(p)
}

@inlinable
public func callNullable(_ p: UnsafeMutableBufferPointer<CInt>?) {
  nullable(p)
}

