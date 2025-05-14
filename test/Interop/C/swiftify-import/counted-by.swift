// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %target-swift-ide-test -print-module -module-to-print=CountedByClang -plugin-path %swift-plugin-dir -I %S/Inputs -source-filename=x -enable-experimental-feature SafeInteropWrappers | %FileCheck %s

// swift-ide-test doesn't currently typecheck the macro expansions, so run the compiler as well
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/CountedBy.swiftmodule -I %S/Inputs -enable-experimental-feature SafeInteropWrappers %s

// Check that ClangImporter correctly infers and expands @_SwiftifyImport macros for functions with __counted_by parameters.

import CountedByClang

// CHECK:      @_alwaysEmitIntoClient public func bitshift(_ m: Int32, _ n: Int32, _ o: Int32, _ p: UnsafeMutableBufferPointer<Int32>)
// CHECK-NEXT: @_alwaysEmitIntoClient public func bitwise(_ m: Int32, _ n: Int32, _ o: Int32, _ p: UnsafeMutableBufferPointer<Int32>)
// CHECK-NEXT: @_alwaysEmitIntoClient public func complexExpr(_ len: Int{{.*}}, _ offset: Int{{.*}}, _ p: UnsafeMutableBufferPointer<Int{{.*}}>)
// CHECK-NEXT: @_alwaysEmitIntoClient public func constFloatCastedToInt(_ p: UnsafeMutableBufferPointer<Int32>)
// CHECK-NEXT: @_alwaysEmitIntoClient public func constInt(_ p: UnsafeMutableBufferPointer<Int32>)
// CHECK-NEXT: @_alwaysEmitIntoClient public func nonnull(_  p: UnsafeMutableBufferPointer<Int{{.*}}>)
// CHECK-NEXT: @_alwaysEmitIntoClient public func nullUnspecified(_  p: UnsafeMutableBufferPointer<Int{{.*}}>)
// CHECK-NEXT: @_alwaysEmitIntoClient public func nullable(_  p: UnsafeMutableBufferPointer<Int{{.*}}>?)
// CHECK-NEXT: @_alwaysEmitIntoClient public func offByOne(_ len: Int32, _ p: UnsafeMutableBufferPointer<Int32>)
// CHECK-NEXT: @_alwaysEmitIntoClient public func offBySome(_ len: Int32, _ offset: Int32, _ p: UnsafeMutableBufferPointer<Int32>)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func returnPointer(_  len: Int{{.*}}) -> UnsafeMutableBufferPointer<Int{{.*}}>
// CHECK-NEXT: @_alwaysEmitIntoClient public func scalar(_ m: Int32, _ n: Int32, _ p: UnsafeMutableBufferPointer<Int32>)
// CHECK-NEXT: @_alwaysEmitIntoClient public func shared(_ len: Int{{.*}}, _ p1: UnsafeMutableBufferPointer<Int{{.*}}>, _ p2: UnsafeMutableBufferPointer<Int{{.*}}>)
// CHECK-NEXT: @_alwaysEmitIntoClient public func simple(_  p: UnsafeMutableBufferPointer<Int{{.*}}>)
// CHECK-NEXT: @_alwaysEmitIntoClient public func simpleFlipped(_  p: UnsafeMutableBufferPointer<Int{{.*}}>)
// CHECK-NEXT: @_alwaysEmitIntoClient public func sizeofParam(_  p: UnsafeMutableBufferPointer<Int{{.*}}>)
// CHECK-NEXT: @_alwaysEmitIntoClient public func sizeofType(_  p: UnsafeMutableBufferPointer<Int{{.*}}>)
// CHECK-NEXT: @_alwaysEmitIntoClient public func swiftAttr(_  p: UnsafeMutableBufferPointer<Int{{.*}}>)

@inlinable
public func callComplexExpr(_ p: UnsafeMutableBufferPointer<CInt>) {
  complexExpr(CInt(p.count), 1, p)
}

@inlinable
public func callConstInt(_ p: UnsafeMutableBufferPointer<CInt>) {
  constInt(p)
}

@inlinable
public func callNonnull(_ p: UnsafeMutableBufferPointer<CInt>) {
  nonnull(p)
}

@inlinable
public func callNullUnspecified(_ p: UnsafeMutableBufferPointer<CInt>) {
  nullUnspecified(p)
}

@inlinable
public func callNullable(_ p: UnsafeMutableBufferPointer<CInt>?) {
  nullable(p)
}

@inlinable
public func callOffByOne(_ p: UnsafeMutableBufferPointer<CInt>) {
  offByOne(0, p)
}

@inlinable
public func callReturnPointer() {
  let a: UnsafeMutableBufferPointer<CInt>? = returnPointer(4) // call wrapper
  let b: UnsafeMutablePointer<CInt>? = returnPointer(4) // call unsafe interop
}

@inlinable
public func callScalar(_ p: UnsafeMutableBufferPointer<CInt>) {
  scalar(4, 2, p)
}

@inlinable
public func callShared(_ p: UnsafeMutableBufferPointer<CInt>, _ p2: UnsafeMutableBufferPointer<CInt>) {
  shared(CInt(p.count), p, p2)
}

@inlinable
public func callSimple(_ p: UnsafeMutableBufferPointer<CInt>) {
  simple(p)
}

@inlinable
public func callSimpleFlipped(_ p: UnsafeMutableBufferPointer<CInt>) {
  simpleFlipped(p)
}

@inlinable
public func callSwiftAttr(_ p: UnsafeMutableBufferPointer<CInt>) {
  swiftAttr(p)
}
