// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %target-swift-ide-test -target arm64-apple-macos -print-module -module-to-print=SizedByNoEscapeClang -plugin-path %swift-plugin-dir -I %S/Inputs -source-filename=x -enable-experimental-feature SafeInteropWrappers | %FileCheck %s

// swift-ide-test doesn't currently typecheck the macro expansions, so run the compiler as well
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target arm64-apple-macos -emit-module -plugin-path %swift-plugin-dir -o %t/SizedByNoEscape.swiftmodule -I %S/Inputs -enable-experimental-feature SafeInteropWrappers %s

// Check that ClangImporter correctly infers and expands @_SwiftifyImport macros for functions with __sized_by __noescape parameters.
import SizedByNoEscapeClang

// CHECK:      @available(macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient public func complexExpr(_ len: Int{{.*}}, _ offset: Int{{.*}}, _ p: RawSpan)
// CHECK-NEXT: @available(macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient public func nonnull(_ p: RawSpan)
// CHECK-NEXT: @available(macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient public func nullUnspecified(_  p: RawSpan)
// CHECK-NEXT: @available(macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient public func nullable(_  p: RawSpan?)
// CHECK-NEXT: @available(macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient public func opaque(_  p: RawSpan)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func returnPointer(_ len: Int{{.*}}) -> UnsafeRawBufferPointer
// CHECK-NEXT: @available(macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient public func shared(_ len: Int{{.*}}, _ p1: RawSpan, _ p2: RawSpan)
// CHECK-NEXT: @available(macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient public func simple(_  p: RawSpan)
// CHECK-NEXT: @available(macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient public func swiftAttr(_  p: RawSpan)

@available(macOS 10.14.4, *)
@inlinable
public func callComplexExpr(_ p: RawSpan) {
  complexExpr(CInt(p.byteCount), 1, p)
}

@available(macOS 10.14.4, *)
@inlinable
public func callNonnull(_ p: RawSpan) {
  nonnull(p)
}

@available(macOS 10.14.4, *)
@inlinable
public func callNullUnspecified(_ p: RawSpan) {
  nullUnspecified(p)
}

@available(macOS 10.14.4, *)
@inlinable
public func callNullable(_ p: RawSpan?) {
  nullable(p)
}

@available(macOS 10.14.4, *)
@inlinable
public func callReturnPointer() {
  let a: UnsafeRawBufferPointer? = returnPointer(4) // call wrapper
  let b: UnsafeRawPointer? = returnPointer(4) // call unsafe interop
}

@available(macOS 10.14.4, *)
@inlinable
public func callShared(_ p: RawSpan, _ p2: RawSpan) {
  shared(CInt(p.byteCount), p, p2)
}

@available(macOS 10.14.4, *)
@inlinable
public func callSimple(_ p: RawSpan) {
  simple(p)
}

@available(macOS 10.14.4, *)
@inlinable
public func callSwiftAttr(_ p: RawSpan) {
  swiftAttr(p)
}