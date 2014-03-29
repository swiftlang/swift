// RUN: %swift -emit-silgen %s | FileCheck %s

@objc class TakesCPointers {
  @objc func constPointer(x: CConstPointer<Int>) {}
  // CHECK-LABEL: sil @_TToFCSo14TakesCPointers12constPointerfS_FT1xGVSs13CConstPointerSi__T_
  // CHECK:         [[CONVERT:%.*]] = function_ref @_TFSs35convertUnsafePointerToCConstPointerU__FT1pGVSs13UnsafePointerQ___GVSs13CConstPointerQ__
  // CHECK:         apply [transparent] [[CONVERT]]<Int>

  @objc func mutablePointer(x: CMutablePointer<Int>) {}
  // CHECK-LABEL: sil @_TToFCSo14TakesCPointers14mutablePointerfS_FT1xGVSs15CMutablePointerSi__T_
  // CHECK:         [[CONVERT:%.*]] = function_ref @_TFSs37convertUnsafePointerToCMutablePointerU__FT1pGVSs13UnsafePointerQ___GVSs15CMutablePointerQ__
  // CHECK:         apply [transparent] [[CONVERT]]<Int>
}

func callWithCPointers(o: TakesCPointers) {
// CHECK-LABEL: sil @_TF18c_pointer_bridging17callWithCPointersFT1oCSo14TakesCPointers_T_

  var arr = [1, 2, 3]

  o.constPointer(arr)
  // -- The CPointer value must be lifetime-fixed for the duration of the
  //    bridged call.
  // CHECK: [[METHOD:%.*]] = class_method [volatile] %0 : $TakesCPointers, #TakesCPointers.constPointer!1.foreign
  // CHECK: [[C_POINTER:%.*]] = apply {{.*}} -> @owned CConstPointer
  // CHECK: [[C_POINTER_FIXED:%.*]] = copy_value [[C_POINTER]]
  // CHECK: [[CONVERT:%.*]] = function_ref @_TFSs35convertCConstPointerToUnsafePointerU__FT1pGVSs13CConstPointerQ___GVSs13UnsafePointerQ__
  // CHECK: [[CONVERTED:%.*]] = apply [transparent] [[CONVERT]]<Int>([[C_POINTER]])
  // CHECK: apply [[METHOD]]([[CONVERTED]], %0)
  // CHECK: fix_lifetime [[C_POINTER_FIXED]]
  // CHECK: destroy_value [[C_POINTER_FIXED]]

  o.mutablePointer(&arr)
  // CHECK: [[METHOD:%.*]] = class_method [volatile] %0 : $TakesCPointers, #TakesCPointers.mutablePointer!1.foreign
  // CHECK: [[C_POINTER:%.*]] = apply {{.*}} -> @owned CMutablePointer
  // CHECK: [[C_POINTER_FIXED:%.*]] = copy_value [[C_POINTER]]
  // CHECK: [[CONVERT:%.*]] = function_ref @_TFSs37convertCMutablePointerToUnsafePointerU__FT1pGVSs15CMutablePointerQ___GVSs13UnsafePointerQ__
  // CHECK: [[CONVERTED:%.*]] = apply [transparent] [[CONVERT]]<Int>([[C_POINTER]])
  // CHECK: apply [[METHOD]]([[CONVERTED]], %0)
  // CHECK: fix_lifetime [[C_POINTER_FIXED]]
  // CHECK: destroy_value [[C_POINTER_FIXED]]
}
