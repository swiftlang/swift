// RUN: %target-swift-emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -enable-sil-ownership | %FileCheck %s

// REQUIRES: objc_interop

import gizmo

// Although we don't ever expose initializers and methods of generic classes
// to ObjC yet, a generic subclass of an ObjC class must still use ObjC
// deallocation.

// CHECK-NOT: sil hidden @$SSo7GenericCfd
// CHECK-NOT: sil hidden @$SSo8NSObjectCfd

class Generic<T>: NSObject {
  var x: Int = 10

  // CHECK-LABEL: sil hidden @$S18objc_generic_class7GenericCfD : $@convention(method) <T> (@owned Generic<T>) -> () {
  // CHECK:       bb0({{%.*}} : @owned $Generic<T>):
  // CHECK: } // end sil function '$S18objc_generic_class7GenericCfD'
  // CHECK-LABEL: sil hidden [thunk] @$S18objc_generic_class7GenericCfDTo : $@convention(objc_method) <T> (Generic<T>) -> () {
  // CHECK:       bb0([[SELF:%.*]] : @unowned $Generic<T>):
  // CHECK:         [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:         [[NATIVE:%.*]] = function_ref @$S18objc_generic_class7GenericCfD
  // CHECK:         apply [[NATIVE]]<T>([[SELF_COPY]])
  // CHECK:       } // end sil function '$S18objc_generic_class7GenericCfDTo'
  deinit {
    // Don't blow up when 'self' is referenced inside an @objc deinit method
    // of a generic class. <rdar://problem/16325525>
    self.x = 0
  }
}

// CHECK-NOT: sil hidden @$S18objc_generic_class7GenericCfd
// CHECK-NOT: sil hidden @$SSo8NSObjectCfd

// CHECK-LABEL: sil hidden @$S18objc_generic_class11SubGeneric1CfD : $@convention(method) <U, V> (@owned SubGeneric1<U, V>) -> () {
// CHECK:       bb0([[SELF:%.*]] : @owned $SubGeneric1<U, V>):
// CHECK:         [[SUPER_DEALLOC:%.*]] = objc_super_method [[SELF]] : $SubGeneric1<U, V>, #Generic.deinit!deallocator.1.foreign : <T> (Generic<T>) -> () -> (), $@convention(objc_method) <τ_0_0> (Generic<τ_0_0>) -> ()
// CHECK:         [[SUPER:%.*]] = upcast [[SELF:%.*]] : $SubGeneric1<U, V> to $Generic<Int>
// CHECK:         apply [[SUPER_DEALLOC]]<Int>([[SUPER]])
class SubGeneric1<U, V>: Generic<Int> {
}

