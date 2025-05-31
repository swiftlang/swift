// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_LifetimeDependence

// RUN: %target-swift-ide-test -print-module -module-to-print=SizedByNoEscapeClang -plugin-path %swift-plugin-dir -I %S/Inputs -source-filename=x -enable-experimental-feature LifetimeDependence -enable-experimental-feature SafeInteropWrappers -Xcc -Wno-nullability-completeness | %FileCheck %s

// swift-ide-test doesn't currently typecheck the macro expansions, so run the compiler as well
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/SizedByNoEscape.swiftmodule -I %S/Inputs -enable-experimental-feature LifetimeDependence -enable-experimental-feature SafeInteropWrappers -strict-memory-safety -warnings-as-errors -Xcc -Werror -Xcc -Wno-nullability-completeness %s

// Check that ClangImporter correctly infers and expands @_SwiftifyImport macros for functions with __sized_by __noescape parameters.
import SizedByNoEscapeClang


// CHECK:      /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func bytesized(_ _bytesized_param1: RawSpan)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(_charsized_param0: copy _charsized_param0)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func charsized(_  _charsized_param0: inout MutableRawSpan)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func complexExpr(_ len: Int{{.*}}, _ offset: Int{{.*}}, _ p: RawSpan)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func nonnull(_ p: RawSpan)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func nullUnspecified(_  p: RawSpan)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func nullable(_  p: RawSpan?)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func opaque(_  p: RawSpan)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func returnPointer(_ len: Int{{.*}}) -> UnsafeRawBufferPointer

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func shared(_ p1: RawSpan, _ p2: RawSpan)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func simple(_  p: RawSpan)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func swiftAttr(_  p: RawSpan)

@available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@inlinable
public func callComplexExpr(_ p: RawSpan) {
  complexExpr(CInt(p.byteCount), 1, p)
}

@available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@inlinable
public func callNonnull(_ p: RawSpan) {
  nonnull(p)
}

@available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@inlinable
public func callNullUnspecified(_ p: RawSpan) {
  nullUnspecified(p)
}

@available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@inlinable
public func callNullable(_ p: RawSpan?) {
  nullable(p)
}

@available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@inlinable
public func callReturnPointer() {
  let _: UnsafeRawBufferPointer? = returnPointer(4) // call wrapper
  let _: UnsafeRawPointer? = returnPointer(4) // call unsafe interop
}

@available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@inlinable
public func callShared(_ p: RawSpan, _ p2: RawSpan) {
  shared(p, p2)
}

@available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@inlinable
public func callSimple(_ p: RawSpan) {
  simple(p)
}

@available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@inlinable
public func callSwiftAttr(_ p: RawSpan) {
  swiftAttr(p)
}

@available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@inlinable
public func callBytesized(_ p: RawSpan) {
  bytesized(p)
}

@available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@inlinable
@lifetime(p: copy p)
public func callCharsized(_ p: inout MutableRawSpan) {
  charsized(&p)
}
