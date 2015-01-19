// RUN: %target-swift-frontend -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -emit-silgen | FileCheck %s

// REQUIRES: objc_interop

import gizmo

// Although we don't ever expose initializers and methods of generic classes
// to ObjC yet, a generic subclass of an ObjC class must still use ObjC
// deallocation.

// CHECK-NOT: sil hidden @_TFCSo7Genericd
// CHECK-NOT: sil hidden @_TFCSo8NSObjectd

class Generic<T>: NSObject {
  var x: Int = 10

  // CHECK-LABEL: sil hidden @_TFC18objc_generic_class7GenericD : $@cc(method) @thin <T> (@owned Generic<T>) -> () {
  // CHECK-NEXT:  bb0({{%.*}} : $Generic<T>):
  // CHECK-LABEL: sil hidden @_TToFC18objc_generic_class7GenericD : $@cc(objc_method) @thin <T> (Generic<T>) -> () {
  // CHECK-NEXT:  bb0([[SELF:%.*]] : $Generic<T>):
  // CHECK:         [[NATIVE:%.*]] = function_ref @_TFC18objc_generic_class7GenericD
  // CHECK:         apply [[NATIVE]]<T>([[SELF]])
  deinit {
    // Don't blow up when 'self' is referenced inside an @objc deinit method
    // of a generic class. <rdar://problem/16325525>
    self.x = 0
  }
}

// CHECK-NOT: sil hidden @_TFC18objc_generic_class7Genericd
// CHECK-NOT: sil hidden @_TFCSo8NSObjectd

// CHECK-LABEL: sil hidden @_TFC18objc_generic_class11SubGeneric1D : $@cc(method) @thin <U, V> (@owned SubGeneric1<U, V>) -> () {
// CHECK-NEXT:  bb0([[SELF:%.*]] : $SubGeneric1<U, V>):
// CHECK:         [[SUPER_DEALLOC:%.*]] = super_method [[SELF]] : $SubGeneric1<U, V>, #Generic.deinit!deallocator.foreign : <T> Generic<T> -> () , $@cc(objc_method) @thin <τ_0_0> (Generic<τ_0_0>) -> ()
// CHECK:         [[SUPER:%.*]] = upcast [[SELF:%.*]] : $SubGeneric1<U, V> to $Generic<Int>
// CHECK:         apply [[SUPER_DEALLOC]]<Int>([[SUPER]])
class SubGeneric1<U, V>: Generic<Int> {
}

