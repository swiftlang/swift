// RUN: %swift -emit-sil -parse-as-library %s | FileCheck %s

oneof Boolish {
  falsy, truthy
}

// CHECK: sil @truthy.getter
var truthy : Boolish {
  // CHECK: [[BOOLISH:%[0-9]+]] = metatype $Boolish.metatype
  // CHECK: [[TRUTHY:%[0-9]+]] = constant_ref ${{.*}}, @truthy.oneofelt
  // CHECK: [[RESULT:%[0-9]+]] = apply [[TRUTHY]]([[BOOLISH]])
  // CHECK: return ([[RESULT]])
  return Boolish.truthy
}

// CHECK: sil @falsy.getter
var falsy : Boolish {
  // CHECK: [[BOOLISH:%[0-9]+]] = metatype $Boolish.metatype
  // CHECK: [[FALSY:%[0-9]+]] = constant_ref ${{.*}}, @falsy.oneofelt
  // CHECK: [[RESULT:%[0-9]+]] = apply [[FALSY]]([[BOOLISH]])
  // CHECK: return ([[RESULT]])
  return Boolish.falsy
}
