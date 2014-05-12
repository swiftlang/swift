// RUN: %swift -emit-silgen %s | FileCheck %s

@objc class TakesCPointers {
  @objc func constPointer(x: CConstPointer<Int>) {}
  // CHECK-LABEL: sil @_TToFC18c_pointer_bridging14TakesCPointers12constPointer{{.*}}
  // CHECK:         [[CONVERT:%.*]] = function_ref @_TFSs36_convertUnsafePointerToCConstPointer
  // CHECK:         apply [transparent] [[CONVERT]]<Int>

  @objc func mutablePointer(x: CMutablePointer<Int>) {}
  // CHECK-LABEL: sil @_TFC18c_pointer_bridging14TakesCPointers14mutablePointer{{.*}}
  // CHECK:         [[CONVERT:%.*]] = function_ref @_TFSs38_convertUnsafePointerToCMutablePointer
  // CHECK:         apply [transparent] [[CONVERT]]<Int>

  @objc func constVoidPointer(x: CConstVoidPointer) {}
  // CHECK-LABEL: sil @_TToFC18c_pointer_bridging14TakesCPointers16constVoidPointer{{.*}} : $@cc(objc_method) @thin (COpaquePointer, TakesCPointers) -> ()
  // CHECK:         [[CONVERT:%.*]] = function_ref @_TFSs41_convertCOpaquePointerToCConstVoidPointer
  // CHECK:         apply [[CONVERT]]

  @objc func mutableVoidPointer(x: CMutableVoidPointer) {}
  // CHECK-LABEL: sil @_TToFC18c_pointer_bridging14TakesCPointers18mutableVoidPointer{{.*}} : $@cc(objc_method) @thin (COpaquePointer, TakesCPointers) -> ()
  // CHECK:         [[CONVERT:%.*]] = function_ref @_TFSs43_convertCOpaquePointerToCMutableVoidPointer
  // CHECK:         apply [[CONVERT]]

  @objc func objcMutablePointer(x: ObjCMutablePointer<TakesCPointers?>) {}
  // CHECK-LABEL: sil @_TToFC18c_pointer_bridging14TakesCPointers18objcMutablePointer{{.*}} : $@cc(objc_method) @thin (UnsafePointer<Optional<TakesCPointers>>, TakesCPointers) -> ()
  // CHECK:         [[CONVERT:%.*]] = function_ref @_TFSs41_convertUnsafePointerToObjCMutablePointer
  // CHECK:         apply [transparent] [[CONVERT]]<Optional<TakesCPointers>>
}

func callWithCPointers(o: TakesCPointers) {
// CHECK-LABEL: sil @_TF18c_pointer_bridging17callWithCPointers{{.*}}

  var arr = [1, 2, 3]

  o.constPointer(arr)
  // -- The CPointer value must be lifetime-fixed for the duration of the
  //    bridged call.
  // CHECK: [[METHOD:%.*]] = class_method [volatile] %0 : $TakesCPointers, #TakesCPointers.constPointer!1.foreign
  // CHECK: [[C_POINTER:%.*]] = apply {{.*}} -> @owned CConstPointer
  // CHECK: retain_value [[C_POINTER]]
  // CHECK: [[CONVERT:%.*]] = function_ref @_TFSs36_convertCConstPointerToUnsafePointer
  // CHECK: [[CONVERTED:%.*]] = apply [transparent] [[CONVERT]]<Int>([[C_POINTER]])
  // CHECK: apply [[METHOD]]([[CONVERTED]], %0)
  // CHECK: fix_lifetime [[C_POINTER]]
  // CHECK: release_value [[C_POINTER]]

  o.mutablePointer(&arr)
  // CHECK: [[METHOD:%.*]] = class_method [volatile] %0 : $TakesCPointers, #TakesCPointers.mutablePointer!1.foreign
  // CHECK: [[C_POINTER:%.*]] = apply {{.*}} -> @owned CMutablePointer
  // CHECK: retain_value [[C_POINTER]]
  // CHECK: [[CONVERT:%.*]] = function_ref @_TFSs38_convertCMutablePointerToUnsafePointer
  // CHECK: [[CONVERTED:%.*]] = apply [transparent] [[CONVERT]]<Int>([[C_POINTER]])
  // CHECK: apply [[METHOD]]([[CONVERTED]], %0)
  // CHECK: fix_lifetime [[C_POINTER]]
  // CHECK: release_value [[C_POINTER]]

  o.constVoidPointer(arr)
  // CHECK: [[METHOD:%.*]] = class_method [volatile] %0 : $TakesCPointers, #TakesCPointers.constVoidPointer!1.foreign
  // CHECK: [[C_POINTER:%.*]] = apply {{.*}} -> @owned CConstVoidPointer
  // CHECK: retain_value [[C_POINTER]]
  // CHECK: [[CONVERT:%.*]] = function_ref @_TFSs41_convertCConstVoidPointerToCOpaquePointer
  // CHECK: [[CONVERTED:%.*]] = apply [[CONVERT]]([[C_POINTER]])
  // CHECK: apply [[METHOD]]([[CONVERTED]], %0)
  // CHECK: fix_lifetime [[C_POINTER]]
  // CHECK: release_value [[C_POINTER]]

  o.mutableVoidPointer(&arr)
  // CHECK: [[METHOD:%.*]] = class_method [volatile] %0 : $TakesCPointers, #TakesCPointers.mutableVoidPointer!1.foreign
  // CHECK: [[C_POINTER:%.*]] = apply {{.*}} -> @owned CMutableVoidPointer
  // CHECK: retain_value [[C_POINTER]]
  // CHECK: [[CONVERT:%.*]] = function_ref @_TFSs43_convertCMutableVoidPointerToCOpaquePointer
  // CHECK: [[CONVERTED:%.*]] = apply [[CONVERT]]([[C_POINTER]])
  // CHECK: apply [[METHOD]]([[CONVERTED]], %0)
  // CHECK: fix_lifetime [[C_POINTER]]
  // CHECK: release_value [[C_POINTER]]

  var p: TakesCPointers? = nil
  o.objcMutablePointer(&p)
  // CHECK: [[METHOD:%.*]] = class_method [volatile] %0 : $TakesCPointers, #TakesCPointers.objcMutablePointer!1.foreign
  // CHECK: [[OBJC_POINTER:%.*]] = apply {{.*}} -> ObjCMutablePointer
  // CHECK: [[CONVERT:%.*]] = function_ref @_TFSs41_convertObjCMutablePointerToUnsafePointer
  // CHECK: [[CONVERTED:%.*]] = apply [transparent] [[CONVERT]]<Optional<TakesCPointers>>([[OBJC_POINTER]])
  // CHECK; apply [[METHOD]]([[CONVERTED]], %0)
  // -- ObjCMutablePointer is trivial, so SILBuilder should swallow the
  //    fix_lifetime
  // CHECK-NOT: fix_lifetime [[OBJC_POINTER]]
}


