// RUN: %target-swift-frontend -module-name test -primary-file %s -import-objc-header %S/Inputs/let_properties_opts.h -O -wmo -emit-sil | %FileCheck %s

// REQUIRES: objc_interop

// CHECK-LABEL: @_objcImplementation extension ObjcInterface
@_objcImplementation extension ObjcInterface {
    final public let i: Int
}

// Check that it doesn't crash with properties in ObjC extensions

// CHECK-LABEL: sil @$s4test0A13ObjcInterfaceySiSo0bC0CF
// CHECK:         ref_element_addr [immutable] %0 : $ObjcInterface, #ObjcInterface.i
// CHECK:       } // end sil function '$s4test0A13ObjcInterfaceySiSo0bC0CF'
public func testObjcInterface(_ x: ObjcInterface) -> Int {
  return x.i
}

