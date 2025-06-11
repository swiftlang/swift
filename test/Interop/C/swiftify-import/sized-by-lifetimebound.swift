// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_LifetimeDependence

// RUN: %target-swift-ide-test -print-module -module-to-print=SizedByLifetimeboundClang -plugin-path %swift-plugin-dir -I %S/Inputs -source-filename=x -enable-experimental-feature SafeInteropWrappers -Xcc -Wno-nullability-completeness | %FileCheck %s

// swift-ide-test doesn't currently typecheck the macro expansions, so run the compiler as well
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/SizedByLifetimebound.swiftmodule -I %S/Inputs -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature LifetimeDependence -strict-memory-safety -warnings-as-errors -Xcc -Werror -strict-memory-safety -warnings-as-errors -Xcc -Werror -Xcc -Wno-nullability-completeness %s

// Check that ClangImporter correctly infers and expands @_SwiftifyImport macros for functions with __sized_by __lifetimebound parameters and return values.

import SizedByLifetimeboundClang

// CHECK:      /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(copy _bytesized_param1)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func bytesized(_ _bytesized_param1: RawSpan) -> MutableRawSpan

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(copy _charsized_param0)
// CHECK-NEXT: @lifetime(_charsized_param0: copy _charsized_param0)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func charsized(_ _charsized_param0: inout MutableRawSpan) -> MutableRawSpan

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func complexExpr(_ len: Int32, _ offset: Int32, _ p: RawSpan) -> RawSpan

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func nonnull(_ len: Int32, _ p: RawSpan) -> RawSpan

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(borrow p)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func nonsizedLifetime(_ len: Int32, _ p: UnsafeRawPointer!) -> RawSpan

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func nullUnspecified(_ len: Int32, _ p: RawSpan) -> RawSpan

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func nullable(_ len: Int32, _ p: RawSpan?) -> RawSpan?

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func opaque(_ len: Int32, _ p: RawSpan) -> RawSpan

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func shared(_ p: RawSpan) -> RawSpan

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func simple(_ len: Int32, _ p: RawSpan) -> RawSpan


@available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@inlinable
public func callComplexExpr(_ p: RawSpan) {
  let _: RawSpan = complexExpr(73, 37, p)
}

@available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@inlinable
public func callNonnull(_ p: RawSpan) {
  let _: RawSpan = nonnull(73, p)
}

@available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@inlinable
public func callNullUnspecified(_ p: RawSpan) {
  let _: RawSpan = nullUnspecified(73, p)
}

@available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@inlinable
public func callNullable(_ p: RawSpan?) {
  let _: RawSpan = nullable(73, p)!
}

@available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@inlinable
public func callShared(_ p: RawSpan) {
  let _: RawSpan = shared(p)
}

@available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@inlinable
public func callSimple(_ p: RawSpan) {
  let _: RawSpan = simple(73, p)
}

@available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@inlinable
public func callNonsizedLifetime(_ p: UnsafeRawPointer) {
  let _: RawSpan = unsafe nonsizedLifetime(73, p)
}

@available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@inlinable
public func callBytesized(_ p: RawSpan) {
  let _: MutableRawSpan = bytesized(p)
}

@available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@inlinable
@lifetime(p: copy p)
public func callCharsized(_ p: inout MutableRawSpan) {
  let _: MutableRawSpan = charsized(&p)
}
