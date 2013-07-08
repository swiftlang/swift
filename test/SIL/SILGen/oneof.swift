// RUN: %swift -emit-sil -parse-as-library %s | FileCheck %s

oneof Boolish {
  falsy, truthy
}

// CHECK: sil @_T5oneof6truthyOS_7Boolishg
var truthy : Boolish {
  // CHECK: [[TRUTHY:%[0-9]+]] = function_ref @_TO5oneof7Boolish6truthyFMS0_S0_
  // CHECK: [[BOOLISH:%[0-9]+]] = metatype $Boolish.metatype
  // CHECK: [[RESULT:%[0-9]+]] = apply [[TRUTHY]]([[BOOLISH]])
  // CHECK: return [[RESULT]]
  return Boolish.truthy
}

// CHECK: sil @_T5oneof5falsyOS_7Boolishg
var falsy : Boolish {
  // CHECK: [[FALSY:%[0-9]+]] = function_ref @_TO5oneof7Boolish5falsyFMS0_S0_
  // CHECK: [[BOOLISH:%[0-9]+]] = metatype $Boolish.metatype
  // CHECK: [[RESULT:%[0-9]+]] = apply [[FALSY]]([[BOOLISH]])
  // CHECK: return [[RESULT]]
  return Boolish.falsy
}
