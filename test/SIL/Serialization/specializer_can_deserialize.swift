// RUN: rm -rf %t; mkdir -p %t; %swift -emit-module %S/Inputs/specializer_input.swift -O2 -sil-serialize-all -parse-stdlib -parse-as-library -emit-module -o %t/Swift.swiftmodule -module-name=Swift -module-link-name swift_stdlib_core -sil-inline-threshold 0
// RUN: %swift %s -O2 -I=%t -emit-sil -o - -sil-inline-threshold 0 | FileCheck %s

import Swift

// Make sure the specializer can deserialize code.

// CHECK-LABEL: sil private @top_level_code : $@thin () -> () {
// CHECK: bb0:
// CHECK: function_ref @_TTSBi32____TFVSs9ContainerCU__fMGS_Q__FT_GS_Q__
// CHECK: function_ref @_TTSBi32____TFVSs9Container11doSomethingU__fGS_Q__FT_T_ : $@cc(method) @thin (Container<Builtin.Int32>) -> ()

// CHECK-LABEL: sil shared @_TTSBi32____TFVSs9ContainerCU__fMGS_Q__FT_GS_Q__ : $@thin (@thin Container<Builtin.Int32>.Type) -> Container<Builtin.Int32> {

// CHECK-LABEL: sil shared @_TTSBi32____TFVSs9Container11doSomethingU__fGS_Q__FT_T_ : $@cc(method) @thin (Container<Builtin.Int32>) -> () {

var c = Container<Int>()
c.doSomething()
