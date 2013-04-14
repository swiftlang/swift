// RUN: %swift -parse-as-library -emit-sil %s | FileCheck %s

var x : Int = 0

// CHECK: toplevel
// CHECK:   [[XADDR:%[0-9]+]] = constant_ref ${{.*}}, @x.globaladdress
// CHECK:   integer_literal ${{.*}}, 0
// CHECK:   store {{%.*}} to [[XADDR]]
// CHECK:   return
