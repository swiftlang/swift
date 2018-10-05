// RUN: %target-swift-emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -disable-objc-attr-requires-foundation-module -enable-sil-ownership -enable-objc-interop | %FileCheck %s

import gizmo

@objc class ObjCClass {}

class A {
  // CHECK-LABEL: sil hidden @$s14objc_metatypes1AC3foo{{[_0-9a-zA-Z]*}}F

  // CHECK-LABEL: sil hidden [thunk] @$s14objc_metatypes1AC3fooyAA9ObjCClassCmAFmFTo
  @objc dynamic func foo(_ m: ObjCClass.Type) -> ObjCClass.Type {
    // CHECK: bb0([[M:%[0-9]+]] : @trivial $@objc_metatype ObjCClass.Type, [[SELF:%[0-9]+]] : @unowned $A):
    // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]] : $A
    // CHECK:   [[M_AS_THICK:%[0-9]+]] = objc_to_thick_metatype [[M]] : $@objc_metatype ObjCClass.Type to $@thick ObjCClass.Type
    // CHECK:   [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
    // CHECK:   [[NATIVE_FOO:%[0-9]+]] = function_ref @$s14objc_metatypes1AC3foo{{[_0-9a-zA-Z]*}}F
    // CHECK:   [[NATIVE_RESULT:%[0-9]+]] = apply [[NATIVE_FOO]]([[M_AS_THICK]], [[BORROWED_SELF_COPY]]) : $@convention(method) (@thick ObjCClass.Type, @guaranteed A) -> @thick ObjCClass.Type
    // CHECK:   end_borrow [[BORROWED_SELF_COPY]]
    // CHECK:   destroy_value [[SELF_COPY]]
    // CHECK:   [[OBJC_RESULT:%[0-9]+]] = thick_to_objc_metatype [[NATIVE_RESULT]] : $@thick ObjCClass.Type to $@objc_metatype ObjCClass.Type
    // CHECK:   return [[OBJC_RESULT]] : $@objc_metatype ObjCClass.Type
    // CHECK: } // end sil function '$s14objc_metatypes1AC3fooyAA9ObjCClassCmAFmFTo'
    return m
  }

  // CHECK-LABEL: sil hidden @$s14objc_metatypes1AC3bar{{[_0-9a-zA-Z]*}}FZ

  // CHECK-LABEL: sil hidden [thunk] @$s14objc_metatypes1AC3bar{{[_0-9a-zA-Z]*}}FZTo
  // CHECK: bb0([[SELF:%[0-9]+]] : @trivial $@objc_metatype A.Type):
  // CHECK-NEXT:   [[OBJC_SELF:%[0-9]+]] = objc_to_thick_metatype [[SELF]] : $@objc_metatype A.Type to $@thick A.Type
  // CHECK:   [[BAR:%[0-9]+]] = function_ref @$s14objc_metatypes1AC3bar{{[_0-9a-zA-Z]*}}FZ
  // CHECK-NEXT:   [[RESULT:%[0-9]+]] = apply [[BAR]]([[OBJC_SELF]]) : $@convention(method) (@thick A.Type) -> ()
  // CHECK-NEXT:   return [[RESULT]] : $()
  @objc dynamic class func bar() { }

  @objc dynamic func takeGizmo(_ g: Gizmo.Type) { }

  // CHECK-LABEL: sil hidden @$s14objc_metatypes1AC7callFoo{{[_0-9a-zA-Z]*}}F
  func callFoo() {
    // Make sure we peephole Type/thick_to_objc_metatype.
    // CHECK-NOT: thick_to_objc_metatype
    // CHECK: metatype $@objc_metatype ObjCClass.Type
    foo(ObjCClass.self)
    // CHECK: return
  }
}
