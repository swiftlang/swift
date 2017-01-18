// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend -emit-module %S/Inputs/specializer_input.swift -O -sil-serialize-all -parse-stdlib -parse-as-library -emit-module -o %t/Swift.swiftmodule -module-name=Swift -module-link-name swiftCore 
// RUN: %target-swift-frontend %s -O -I %t -emit-sil -o - | %FileCheck %s

import Swift

// Make sure the specializer can deserialize code.

// CHECK-LABEL: sil @main
// CHECK: bb0({{.*}}):
// CHECK: function_ref @{{.*}}_TTSg5Bi32____TFVs9ContainerCfT_GS_x_
// CHECK: function_ref @{{.*}}_TTSg5Bi32____TFVs9Container11doSomethingfT_T_ 

// CHECK-LABEL: sil shared [noinline] @_TTSf4d___TTSg5Bi32____TFVs9ContainerCfT_GS_x_

// CHECK-LABEL: sil shared [noinline] @_TTSf4d___TTSg5Bi32____TFVs9Container11doSomethingfT_T_

var c = Container<Int>()
c.doSomething()
