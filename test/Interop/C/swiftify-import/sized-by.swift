// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %target-swift-ide-test -print-module -module-to-print=SizedByClang -plugin-path %swift-plugin-dir -I %S/Inputs -source-filename=x -enable-experimental-feature SafeInteropWrappers -Xcc -Werror -Xcc -Wno-nullability-completeness | %FileCheck %s

// swift-ide-test doesn't currently typecheck the macro expansions, so run the compiler as well
// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %S/Inputs -enable-experimental-feature SafeInteropWrappers -strict-memory-safety -warnings-as-errors -Xcc -Werror -Xcc -Wno-nullability-completeness %s

// Check that ClangImporter correctly infers and expands @_SwiftifyImport macros for functions with __sized_by parameters.
import SizedByClang


// CHECK:      /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func aliasedBytesized(_  p: UnsafeMutableRawBufferPointer)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func bytesized(_  size: Int{{.*}}) -> UnsafeMutableRawBufferPointer

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func charsized(_  _charsized_param0: UnsafeMutableRawBufferPointer)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func complexExpr(_ len: Int{{.*}}, _ offset: Int{{.*}}, _ p: UnsafeMutableRawBufferPointer)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func nonnull(_  p: UnsafeMutableRawBufferPointer)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func nullUnspecified(_  p: UnsafeMutableRawBufferPointer)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func nullable(_  p: UnsafeMutableRawBufferPointer?)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func opaque(_  p: UnsafeRawBufferPointer)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func opaqueptr(_  p: UnsafeRawBufferPointer)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func returnPointer(_ len: Int{{.*}}) -> UnsafeMutableRawBufferPointer

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func shared(_ p1: UnsafeMutableRawBufferPointer, _ p2: UnsafeMutableRawBufferPointer)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func simple(_  p: UnsafeMutableRawBufferPointer)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func swiftAttr(_  p: UnsafeMutableRawBufferPointer)

@inlinable
public func callComplexExpr(_ p: UnsafeMutableRawBufferPointer) {
  unsafe complexExpr(CInt(p.count), 1, p)
}

@inlinable
public func callNonnull(_ p: UnsafeMutableRawBufferPointer) {
  unsafe nonnull(p)
}

@inlinable
public func callNullUnspecified(_ p: UnsafeMutableRawBufferPointer) {
  unsafe nullUnspecified(p)
}

@inlinable
public func callNullable(_ p: UnsafeMutableRawBufferPointer?) {
  unsafe nullable(p)
}

@inlinable
public func callOpaque(_ p: UnsafeRawBufferPointer) {
  unsafe opaque(p)
}

@inlinable
public func callOpaqueptr(_ p: UnsafeRawBufferPointer) {
  unsafe opaqueptr(p)
}

@inlinable
public func callReturnPointer() {
  let _: UnsafeMutableRawBufferPointer? = unsafe returnPointer(4) // call wrapper
  let _: UnsafeMutableRawPointer? = unsafe returnPointer(4) // call unsafe interop
}

@inlinable
public func callShared(_ p: UnsafeMutableRawBufferPointer, _ p2: UnsafeMutableRawBufferPointer) {
  unsafe shared(p, p2)
}

@inlinable
public func callSimple(_ p: UnsafeMutableRawBufferPointer) {
  unsafe simple(p)
}

@inlinable
public func callSwiftAttr(_ p: UnsafeMutableRawBufferPointer) {
  unsafe swiftAttr(p)
}

@inlinable
public func callCharsized(_ p: UnsafeMutableRawBufferPointer) {
  unsafe charsized(p)
}

@inlinable
public func callBytesized() {
  let _: UnsafeMutableRawBufferPointer = unsafe bytesized(37)
}

@inlinable
public func callAliasedBytesized(_ p: UnsafeMutableRawBufferPointer) {
  unsafe aliasedBytesized(p)
}
