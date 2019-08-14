// RUN: %target-swift-frontend -module-name cxx_sil -I %S/Inputs/custom-modules -module-cache-path %t -enable-cxx-interop -emit-sil -o - -primary-file %s | %FileCheck %s

import CXXInterop

// CHECK-LABEL: sil hidden @$s7cxx_sil13virtualMethod1as5Int32VSpySo14MethodsVirtualVG_tF
// CHECK: [[INT_LIT:%.*]] = integer_literal $Builtin.Int32, 7
// CHECK: [[INT:%.*]] = struct $Int32 ([[INT_LIT]] : $Builtin.Int32)
// CHECK: [[THIS_PTR:%.*]] = begin_access [modify] [unsafe] {{%.*}} : $*MethodsVirtual
// CHECK: ([[VIRTUAL_METHOD:%.*]], [[THIS_PTR_ADJUST:%.*]]) = cxx_virtual_method [[THIS_PTR]] : $*MethodsVirtual, #MethodsVirtual.SimpleVirtualMethod!1.foreign : (inout MethodsVirtual) -> (Int32) -> Int32, $@convention(c) (@inout MethodsVirtual, Int32) -> Int32
// TODO: Use adjusted this ptr here...
// CHECK: [[RESULT:%.*]] = apply [[VIRTUAL_METHOD]]([[THIS_PTR]], [[INT]]) : $@convention(c) (@inout MethodsVirtual, Int32) -> Int32
// CHECK: return [[RESULT]] : $Int32
func virtualMethod(a: UnsafeMutablePointer<MethodsVirtual>) -> Int32 {
  return a.pointee.SimpleVirtualMethod(7);
}
