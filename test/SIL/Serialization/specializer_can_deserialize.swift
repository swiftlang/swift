// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend -emit-module %S/Inputs/specializer_input.swift -O -sil-serialize-all -parse-stdlib -parse-as-library -emit-module -o %t/Swift.swiftmodule -module-name=Swift -module-link-name swiftCore -sil-inline-threshold 0 -disable-func-sig-opts
// RUN: %target-swift-frontend %s -O -I %t -emit-sil -o - -sil-inline-threshold 0 -disable-func-sig-opts | FileCheck %s

import Swift

// Make sure the specializer can deserialize code.

// CHECK-LABEL: sil @main
// CHECK: bb0({{.*}}):
// CHECK: function_ref @_TTSg5Bi32____TFVSs9ContainerCurfMGS_q__FT_GS_q__
// CHECK: function_ref @_TTSg5Bi32____TFVSs9Container11doSomethingurfGS_q__FT_T_ : $@convention(method) (Container<Builtin.Int32>) -> ()

// CHECK-LABEL: sil shared [fragile] @_TTSg5Bi32____TFVSs9ContainerCurfMGS_q__FT_GS_q__ : $@convention(thin) (@thin Container<Builtin.Int32>.Type) -> Container<Builtin.Int32> {

// CHECK-LABEL: sil shared [fragile] @_TTSg5Bi32____TFVSs9Container11doSomethingurfGS_q__FT_T_ : $@convention(method) (Container<Builtin.Int32>) -> () {

var c = Container<Int>()
c.doSomething()
