// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path=%t/default_arg_other.swiftmodule -module-name=default_arg_other %S/Inputs/default_arg_other.swift
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name default_arg_multiple_modules -I %t %s | %FileCheck %s

import default_arg_other

// CHECK-LABEL: sil hidden [ossa] @${{.*}}test1{{.*}}
func test1() -> Int {
  // CHECK: [[DEF_ARG_FN:%[0-9]+]] = function_ref @$s17default_arg_other3foo1xS2i_tFfA_ : $@convention(thin) () -> Int
  // CHECK: [[DEF_ARG:%[0-9]+]] = apply [[DEF_ARG_FN]]() {{.*}}
  // CHECK: [[FN:%[0-9]+]] = function_ref @$s17default_arg_other3foo1xS2i_tF : $@convention(thin) (Int) -> Int
  // CHECK: [[CALL:%[0-9]+]] = apply [[FN]]([[DEF_ARG]]) {{.*}}
  // CHECK: return [[CALL]] : $Int
  return foo()
}

// CHECK-LABEL: sil hidden [ossa] @${{.*}}test2{{.*}}
func test2() -> Int {
  // CHECK: [[DEF_ARG_FN:%[0-9]+]] = function_ref @$s17default_arg_other10Subscript1VyS2icipfA_ : $@convention(thin) () -> Int
  // CHECK: [[DEF_ARG:%[0-9]+]] = apply [[DEF_ARG_FN]]() {{.*}}
  // CHECK: [[FN:%[0-9]+]] = function_ref @$s17default_arg_other10Subscript1VyS2icig : $@convention(method) (Int, Subscript1) -> Int
  // CHECK: [[CALL:%[0-9]+]] = apply [[FN]]([[DEF_ARG]], {{.*}}
  // CHECK: return [[CALL]] : $Int
  return Subscript1()[]
}

// CHECK-LABEL: sil hidden [ossa] @${{.*}}test3{{.*}}
func test3() -> String {
  // This should not call default arg constructor
  // CHECK: [[STR_LIT:%[0-9]+]] = string_literal utf8 "test3()"
  // CHECK: [[DEF_ARG:%[0-9]+]] = apply %{{[0-9]+}}([[STR_LIT]], {{.*}}
  // CHECK: [[FN:%[0-9]+]] = function_ref @$s17default_arg_other10Subscript2VyS2Scig : $@convention(method) (@guaranteed String, Subscript2) -> @owned String
  // CHECK: [[CALL:%[0-9]+]] = apply [[FN]]([[DEF_ARG]], {{.*}}
  // CHECK: return [[CALL]] : $String
  return Subscript2()[]
}

// CHECK-LABEL: sil hidden [ossa] @${{.*}}test4{{.*}}
func test4() {
  // CHECK: [[DEF_ARG_FN:%[0-9]+]] = function_ref @$s17default_arg_other10Subscript1VyS2icipfA_ : $@convention(thin) () -> Int
  // CHECK: [[DEF_ARG:%[0-9]+]] = apply [[DEF_ARG_FN]]() {{.*}}
  // CHECK: keypath $KeyPath<Subscript1, Int>, (root $Subscript1; gettable_property $Int, id @$s17default_arg_other10Subscript1VyS2icig : $@convention(method) (Int, Subscript1) -> Int, getter @$s17default_arg_other10Subscript1VyS2icipACTK : $@convention(keypath_accessor_getter) (@in_guaranteed Subscript1, @in_guaranteed Int) -> @out Int, indices [%$0 : $Int : $Int], indices_equals @$sSiTH : $@convention(keypath_accessor_equals) (@in_guaranteed Int, @in_guaranteed Int) -> Bool, indices_hash @$sSiTh : $@convention(keypath_accessor_hash) (@in_guaranteed Int) -> Int, external #Subscript1.subscript) ([[DEF_ARG]])
  _ = \Subscript1.[]
}

// CHECK-LABEL: sil hidden [ossa] @${{.*}}test5{{.*}}
func test5() {
  // This should not call default arg constructor
  // CHECK: [[STR_LIT:%[0-9]+]] = string_literal utf8 "test5()"
  // CHECK: [[DEF_ARG:%[0-9]+]] = apply %{{[0-9]+}}([[STR_LIT]], {{.*}}
  // CHECK: keypath $KeyPath<Subscript2, String>, (root $Subscript2; gettable_property $String, id @$s17default_arg_other10Subscript2VyS2Scig : $@convention(method) (@guaranteed String, Subscript2) -> @owned String, getter @$s17default_arg_other10Subscript2VyS2ScipACTK : $@convention(keypath_accessor_getter) (@in_guaranteed Subscript2, @in_guaranteed String) -> @out String, indices [%$0 : $String : $String], indices_equals @$sSSTH : $@convention(keypath_accessor_equals) (@in_guaranteed String, @in_guaranteed String) -> Bool, indices_hash @$sSSTh : $@convention(keypath_accessor_hash) (@in_guaranteed String) -> Int, external #Subscript2.subscript) ([[DEF_ARG]])
  _ = \Subscript2.[]
}
