// RUN: %swift -emit-sil %s | FileCheck %s

struct B {
  var i : Int, j : Float
  var c : C
}

struct C {
  var x : Int
  constructor() { x = 17 }
}

// CHECK: sil @constructor.allocator.1 : $(B.metatype)() -> B {
// CHECK: bb0([[metaThis:%[0-9]+]] : B.metatype):
// CHECK: [[this:%[0-9]+]] = alloc_var stack $B
// Initialize i
// CHECK: [[Int64Ctor:%[0-9]+]] = function_ref $(Int64.metatype)() -> Int64, @constructor.allocator.1
// CHECK: [[Int64Meta:%[0-9]+]] = metatype $Int64.metatype
// CHECK: [[Int64Construct:%[0-9]+]] = apply [[Int64Ctor]]([[Int64Meta]])
// CHECK: [[i:%[0-9]+]] = element_addr [[this]], 0
// CHECK: store [[Int64Construct]] to [[i]]
// Initialize j
// CHECK: [[Float32Ctor:%[0-9]+]] = function_ref $(Float32.metatype)() -> Float32, @constructor.allocator.1
// CHECK: [[Float32Meta:%[0-9]+]] = metatype $Float32.metatype
// CHECK: [[Float32Construct:%[0-9]+]] = apply [[Float32Ctor]]([[Float32Meta]])
// CHECK: [[i:%[0-9]+]] = element_addr [[this]], 1
// CHECK: store [[Float32Construct]] to [[i]]
// Initialize c
// CHECK: [[CCtor:%[0-9]+]] = function_ref $(C.metatype)() -> C, @constructor.allocator.1
// CHECK: [[CMeta:%[0-9]+]] = metatype $C.metatype
// CHECK: [[CConstruct:%[0-9]+]] = apply [[CCtor]]([[CMeta]])
// CHECK: [[i:%[0-9]+]] = element_addr [[this]], 2
// CHECK: store [[CConstruct]] to [[i]]
