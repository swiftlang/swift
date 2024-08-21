// Test doesn't pass on all platforms (rdar://101420862)
// REQUIRES: OS=macosx

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-experimental-feature CImplementation -I %S/Inputs/abi -F %clang-importer-sdk-path/frameworks %s -import-bridging-header %S/Inputs/cdecl_implementation.h -emit-ir -target %target-future-triple > %t.ir
// RUN: %FileCheck --input-file %t.ir %s
// REQUIRES: objc_interop

//
// Functions
//

@_objcImplementation @_cdecl("implFunc")
public func implFunc(_ param: Int32) {}

@_objcImplementation @_cdecl("implFuncCName")
public func implFuncCName(_ param: Int32) {}

public func fn() {
  implFunc(2)
  implFuncCName(3)
}

// implFunc(_:)
// CHECK-LABEL: define void @implFunc
// FIXME: We'd like this to be internal or hidden, not public.
// CHECK: define swiftcc void @"$s20cdecl_implementation8implFuncyys5Int32VF"

// inplFuncCName(_:)
// CHECK-LABEL: define void @"\01_implFuncAsmName"
// FIXME: We'd like this to be internal or hidden, not public.
// CHECK: define swiftcc void @"$s20cdecl_implementation13implFuncCNameyys5Int32VF"

// fn()
// CHECK-LABEL: define swiftcc void @"$s20cdecl_implementation2fnyyF"
// CHECK:   call void @implFunc
// CHECK:   ret void
// CHECK: }
