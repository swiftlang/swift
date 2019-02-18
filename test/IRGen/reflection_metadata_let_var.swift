// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s

// CHECK: [[INT:@.*]] = linkonce_odr hidden constant {{.*}} c"Si"
// CHECK: [[X:@.*]] = private constant {{.*}} c"x\00"
// CHECK: [[STRING:@.*]] = linkonce_odr hidden constant {{.*}} c"SS"
// CHECK: [[Y:@.*]] = private constant {{.*}} c"y\00"

// CHECK-LABEL: @"$s27reflection_metadata_let_var6StruccVMF" = internal constant
struct Strucc {
// --             flags (let)
// CHECK-SAME:    i32 0, 
// --             type
// CHECK-SAME:    [[INT]] to
// --             name
// CHECK-SAME:    [[X]] to
  let x: Int

// --             flags (var)
// CHECK-SAME:    i32 2, 
// --             type
// CHECK-SAME:    [[STRING]] to
// --             name
// CHECK-SAME:    [[Y]] to
  var y: String
}

