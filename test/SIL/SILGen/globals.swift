// RUN: %swift -parse-as-library -emit-sil %s | FileCheck %s

var x : Int = 0

// CHECK: sil internal @top_level_code
// CHECK:   [[XADDR:%[0-9]+]] = global_addr @x : ${{.*}}
// CHECK:   integer_literal ${{.*}}, 0
// CHECK:   store {{%.*}} to [[XADDR]]
// CHECK:   return
