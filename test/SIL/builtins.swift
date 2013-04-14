// RUN: %swift -emit-sil -parse-as-library %s | FileCheck %s

import Builtin

// CHECK: sil @foo
func foo(x:Builtin.Int1, y:Builtin.Int1) -> Builtin.Int1 {
  // CHECK: constant_ref ${{.*}}, @cmp_eq_Int1
  return Builtin.cmp_eq_Int1(x, y)
}
