// RUN: %swift -enable-silgen-lvalue-peepholes -parse-as-library -emit-silgen %s | FileCheck %s

// CHECK: sil @_T21copy_lvalue_peepholes20init_var_from_lvalueFT1xSi_T_
// CHECK:   [[X:%.*]] = alloc_box $Int64
// CHECK:   [[Y:%.*]] = alloc_box $Int64
// CHECK:   copy_addr [[X]]#1 to [initialization] [[Y]]#1 : $*Int64
func init_var_from_lvalue(x:Int) {
  var y = x
}

