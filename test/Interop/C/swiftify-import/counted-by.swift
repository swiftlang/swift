// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %target-swift-ide-test -print-module -module-to-print=CountedByClang -plugin-path %swift-plugin-dir -I %S/Inputs -source-filename=x -enable-experimental-feature SafeInteropWrappers -Xcc -Wno-nullability-completeness | %FileCheck %s

// swift-ide-test doesn't currently typecheck the macro expansions, so run the compiler as well
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/CountedBy.swiftmodule -I %S/Inputs -enable-experimental-feature SafeInteropWrappers -strict-memory-safety -warnings-as-errors -Xcc -Werror -Xcc -Wno-nullability-completeness %s

// Check that ClangImporter correctly infers and expands @_SwiftifyImport macros for functions with __counted_by parameters.

import CountedByClang


// CHECK:      /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func bitshift(_ m: Int32, _ n: Int32, _ o: Int32, _ p: UnsafeMutableBufferPointer<Int32>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func bitwise(_ m: Int32, _ n: Int32, _ o: Int32, _ p: UnsafeMutableBufferPointer<Int32>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func complexExpr(_ len: Int{{.*}}, _ offset: Int{{.*}}, _ p: UnsafeMutableBufferPointer<Int{{.*}}>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func constFloatCastedToInt(_ p: UnsafeMutableBufferPointer<Int32>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func constInt(_ p: UnsafeMutableBufferPointer<Int32>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func nonnull(_  p: UnsafeMutableBufferPointer<Int{{.*}}>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func nullUnspecified(_  p: UnsafeMutableBufferPointer<Int{{.*}}>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func nullable(_  p: UnsafeMutableBufferPointer<Int{{.*}}>?)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func offByOne(_ len: Int32, _ p: UnsafeMutableBufferPointer<Int32>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func offBySome(_ len: Int32, _ offset: Int32, _ p: UnsafeMutableBufferPointer<Int32>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func returnPointer(_  len: Int{{.*}}) -> UnsafeMutableBufferPointer<Int{{.*}}>

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func scalar(_ m: Int32, _ n: Int32, _ p: UnsafeMutableBufferPointer<Int32>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func shared(_ p1: UnsafeMutableBufferPointer<Int{{.*}}>, _ p2: UnsafeMutableBufferPointer<Int{{.*}}>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func simple(_  p: UnsafeMutableBufferPointer<Int{{.*}}>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func simpleFlipped(_  p: UnsafeMutableBufferPointer<Int{{.*}}>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func sizeofParam(_  p: UnsafeMutableBufferPointer<Int{{.*}}>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func sizeofType(_  p: UnsafeMutableBufferPointer<Int{{.*}}>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func swiftAttr(_  p: UnsafeMutableBufferPointer<Int{{.*}}>)

@inlinable
public func callComplexExpr(_ p: UnsafeMutableBufferPointer<CInt>) {
  unsafe complexExpr(CInt(p.count), 1, p)
}

@inlinable
public func callConstInt(_ p: UnsafeMutableBufferPointer<CInt>) {
  unsafe constInt(p)
}

@inlinable
public func callNonnull(_ p: UnsafeMutableBufferPointer<CInt>) {
  unsafe nonnull(p)
}

@inlinable
public func callNullUnspecified(_ p: UnsafeMutableBufferPointer<CInt>) {
  unsafe nullUnspecified(p)
}

@inlinable
public func callNullable(_ p: UnsafeMutableBufferPointer<CInt>?) {
  unsafe nullable(p)
}

@inlinable
public func callOffByOne(_ p: UnsafeMutableBufferPointer<CInt>) {
  unsafe offByOne(0, p)
}

@inlinable
public func callReturnPointer() {
  let _: UnsafeMutableBufferPointer<CInt>? = returnPointer(4) // call wrapper
  let _: UnsafeMutablePointer<CInt>? = returnPointer(4) // call unsafe interop
}

@inlinable
public func callScalar(_ p: UnsafeMutableBufferPointer<CInt>) {
  unsafe scalar(4, 2, p)
}

@inlinable
public func callShared(_ p: UnsafeMutableBufferPointer<CInt>, _ p2: UnsafeMutableBufferPointer<CInt>) {
  unsafe shared(p, p2)
}

@inlinable
public func callSimple(_ p: UnsafeMutableBufferPointer<CInt>) {
  unsafe simple(p)
}

@inlinable
public func callSimpleIndirectOriginal(_ p: UnsafeMutablePointer<CInt>) {
  let f = unsafe simple
  unsafe f(13, p)
}

@inlinable
public func callSimpleIndirectOverload(_ p: UnsafeMutableBufferPointer<CInt>) {
  let f: (UnsafeMutableBufferPointer<CInt>) -> Void = unsafe simple
  unsafe f(p)
}

@inlinable
public func callSimpleFlipped(_ p: UnsafeMutableBufferPointer<CInt>) {
  unsafe simpleFlipped(p)
}

@inlinable
public func callSwiftAttr(_ p: UnsafeMutableBufferPointer<CInt>) {
  unsafe swiftAttr(p)
}
