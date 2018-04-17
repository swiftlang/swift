// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/specializer_input.swift -O -parse-stdlib -parse-as-library -emit-module -o %t/Swift.swiftmodule -module-name=Swift -module-link-name swiftCore 
// RUN: %target-swift-frontend %s -O -I %t -emit-sil -o - | %FileCheck %s

import Swift

// Make sure the specializer can deserialize code.

// CHECK-LABEL: sil @main
// CHECK: bb0({{.*}}):
// CHECK: function_ref @$Ss9ContainerVAByxGycfCBi32__Tg5{{.*}}
// CHECK: function_ref @$Ss9ContainerV11doSomethingyyFBi32__Tg5{{.*}} 

// CHECK-LABEL: sil shared [noinline] @$Ss9ContainerVAByxGycfCBi32__Tg5Tf4d_n

// CHECK-LABEL: sil shared [noinline] @$Ss9ContainerV11doSomethingyyFBi32__Tg5Tf4d_n

var c = Container<Int>()
c.doSomething()
