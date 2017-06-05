// RUN: %target-swift-frontend -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -emit-silgen | %FileCheck %s

// REQUIRES: objc_interop

import gizmo

// Although we don't ever expose initializers and methods of generic classes
// to ObjC yet, a generic subclass of an ObjC class must still use ObjC
// deallocation.

// CHECK-NOT: sil hidden @_T0So7GenericCfd
// CHECK-NOT: sil hidden @_T0So8NSObjectCfd

class Generic<T>: NSObject {
  var x: Int = 10

  // CHECK-LABEL: sil hidden @_T018objc_generic_class7GenericCfD : $@convention(method) <T> (@owned Generic<T>) -> () {
  // CHECK:       bb0({{%.*}} : $Generic<T>):
  // CHECK-LABEL: sil hidden [thunk] @_T018objc_generic_class7GenericCfDTo : $@convention(objc_method) <T> (Generic<T>) -> () {
  // CHECK:       bb0([[SELF:%.*]] : $Generic<T>):
  // CHECK:         [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:         [[NATIVE:%.*]] = function_ref @_T018objc_generic_class7GenericCfD
  // CHECK:         apply [[NATIVE]]<T>([[SELF_COPY]])
  deinit {
    // Don't blow up when 'self' is referenced inside an @objc deinit method
    // of a generic class. <rdar://problem/16325525>
    self.x = 0
  }
}

// CHECK-NOT: sil hidden @_T018objc_generic_class7GenericCfd
// CHECK-NOT: sil hidden @_T0So8NSObjectCfd

// CHECK-LABEL: sil hidden @_T018objc_generic_class11SubGeneric1CfD : $@convention(method) <U, V> (@owned SubGeneric1<U, V>) -> () {
// CHECK:       bb0([[SELF:%.*]] : $SubGeneric1<U, V>):
// CHECK:         [[SUPER_DEALLOC:%.*]] = super_method [[SELF]] : $SubGeneric1<U, V>, #Generic.deinit!deallocator.foreign : <T> (Generic<T>) -> () -> (), $@convention(objc_method) <τ_0_0> (Generic<τ_0_0>) -> ()
// CHECK:         [[SUPER:%.*]] = upcast [[SELF:%.*]] : $SubGeneric1<U, V> to $Generic<Int>
// CHECK:         apply [[SUPER_DEALLOC]]<Int>([[SUPER]])
class SubGeneric1<U, V>: Generic<Int> {
}

