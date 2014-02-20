// RUN: %swift -emit-silgen %s | FileCheck %s

class A {
  // CHECK-LABEL: sil @_TFC14objc_metatypes1A3foofS0_FT1mMS0__MS0_ : $@cc(method) @thin (@thick A.metatype, @owned A) -> @thick A.metatype

  // CHECK-LABEL: sil @_TToFC14objc_metatypes1A3foofS0_FT1mMS0__MS0_ : $@cc(objc_method) @thin (@objc_metatype A.metatype, A) -> @objc_metatype A.metatype
  @objc func foo(m: A.metatype) -> A.metatype { 
    // CHECK: bb0([[M:%[0-9]+]] : $@objc_metatype A.metatype, [[SELF:%[0-9]+]] : $A):
    // CHECK:   strong_retain [[SELF]] : $A
    // CHECK:   [[M_AS_THICK:%[0-9]+]] = objc_to_thick_metatype [[M]] : $@objc_metatype A.metatype to $@thick A.metatype

    // CHECK:   [[NATIVE_FOO:%[0-9]+]] = function_ref @_TFC14objc_metatypes1A3foofS0_FT1mMS0__MS0_ : $@cc(method) @thin (@thick A.metatype, @owned A) -> @thick A.metatype
    // CHECK:   [[NATIVE_RESULT:%[0-9]+]] = apply [[NATIVE_FOO]]([[M_AS_THICK]], [[SELF]]) : $@cc(method) @thin (@thick A.metatype, @owned A) -> @thick A.metatype
    // CHECK:   [[OBJC_RESULT:%[0-9]+]] = thick_to_objc_metatype [[NATIVE_RESULT]] : $@thick A.metatype to $@objc_metatype A.metatype
    // CHECK:   return [[OBJC_RESULT]] : $@objc_metatype A.metatype
    return m 
  }
}
