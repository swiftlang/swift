// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_Lifetimes

// RUN: %target-swift-ide-test -print-module -module-to-print=CountedByLifetimeboundClang -plugin-path %swift-plugin-dir -I %S/Inputs -source-filename=x -enable-experimental-feature SafeInteropWrappers -Xcc -Wno-nullability-completeness | %FileCheck %s

// swift-ide-test doesn't currently typecheck the macro expansions, so run the compiler as well
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/CountedByLifetimebound.swiftmodule -I %S/Inputs -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature Lifetimes -strict-memory-safety -warnings-as-errors -Xcc -Werror -Xcc -Wno-nullability-completeness %s

// Check that ClangImporter correctly infers and expands @_SwiftifyImport macros for functions with __sized_by __lifetimebound parameters and return values.

import CountedByLifetimeboundClang

// CHECK:      /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(copy p)
// CHECK-NEXT: @_lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func complexExpr(_ len: Int32, _ offset: Int32, _ p: inout MutableSpan<Int32>) -> MutableSpan<Int32>

// CHECK:      /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(copy p)
// CHECK-NEXT: @_lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func constant(_ p: inout MutableSpan<Int32>?) -> MutableSpan<Int32>?

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(borrow p)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func noncountedLifetime(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!) -> MutableSpan<Int32>

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(copy p)
// CHECK-NEXT: @_lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func nonnull(_ len: Int32, _ p: inout MutableSpan<Int32>) -> MutableSpan<Int32>

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(copy p)
// CHECK-NEXT: @_lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func nullUnspecified(_ len: Int32, _ p: inout MutableSpan<Int32>) -> MutableSpan<Int32>

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(copy p)
// CHECK-NEXT: @_lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func nullable(_ len: Int32, _ p: inout MutableSpan<Int32>?) -> MutableSpan<Int32>?

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(copy p)
// CHECK-NEXT: @_lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func shared(_ p: inout MutableSpan<Int32>) -> MutableSpan<Int32>

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(copy p)
// CHECK-NEXT: @_lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func simple(_ len: Int32, _ p: inout MutableSpan<Int32>) -> MutableSpan<Int32>


@available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@inlinable
public func callComplexExpr(_ p: inout MutableSpan<CInt>) {
  let _: MutableSpan<CInt> = complexExpr(73, 37, &p)
}

@available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@inlinable
public func callNonnull(_ p: inout MutableSpan<CInt>) {
  let _: MutableSpan<CInt> = nonnull(73, &p)
}

@available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@inlinable
public func callNullUnspecified(_ p: inout MutableSpan<CInt>) {
  let _: MutableSpan<CInt> = nullUnspecified(73, &p)
}

@available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@inlinable
public func callNullable(_ p: inout MutableSpan<CInt>?) {
  let _: MutableSpan<CInt> = nullable(73, &p)!
}

@available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@inlinable
public func callShared(_ p: inout MutableSpan<CInt>) {
  let _: MutableSpan<CInt> = shared(&p)
}

@available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@inlinable
public func callSimple(_ p: inout MutableSpan<CInt>) {
  let _: MutableSpan<CInt> = simple(73, &p)
}

@available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@inlinable
public func callNoncountedLifetime(_ p: UnsafeMutablePointer<CInt>) {
  let _: MutableSpan<CInt> = unsafe noncountedLifetime(73, p)
}

@available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@inlinable
public func callConstant(_ p: inout MutableSpan<CInt>?) {
  let _: MutableSpan<CInt>? = constant(&p)
}
