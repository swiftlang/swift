// RUN: %target-swift-frontend -module-name test -primary-file %s -import-objc-header %S/Inputs/let_properties_opts.h -O -wmo -Xllvm -sil-print-types -emit-sil -target %target-stable-abi-triple | %FileCheck %s

// REQUIRES: objc_interop

// CHECK-LABEL: @_objcImplementation extension ObjcInterface
@_objcImplementation extension ObjcInterface {
    final public let i: Int
}

// Check that it doesn't crash with properties in ObjC extensions

// CHECK-LABEL: sil @$s4test0A13ObjcInterfaceySiSo0bC0CF
// CHECK:         ref_element_addr [immutable] %0 : $ObjcInterface, #ObjcInterface.i
// CHECK-LABEL: } // end sil function '$s4test0A13ObjcInterfaceySiSo0bC0CF'
public func testObjcInterface(_ x: ObjcInterface) -> Int {
  return x.i
}

// Test that private @objc constants aren't optimized, but instead continue to pass through ObjC message dispatch.
@_objcImplementation extension ObjcInterfaceConstInit {
  @objc private let constant: Int = 0

  // CHECK-LABEL: sil hidden @$sSo22ObjcInterfaceConstInitC4testE0E15PrivateConstantSiyF : $@convention(method) (@guaranteed ObjcInterfaceConstInit) -> Int {
  // CHECK: objc_method %0 : $ObjcInterfaceConstInit, #ObjcInterfaceConstInit.constant!getter.foreign
  // CHECK-LABEL: } // end sil function '$sSo22ObjcInterfaceConstInitC4testE0E15PrivateConstantSiyF'
  final func testPrivateConstant() -> Int {
    return constant
  }
}
