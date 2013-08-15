// RUN: %swift -emit-silgen %s | FileCheck %s

// CHECK: sil @_T14break_continue5test1FT1bSb_T_
func test1(b:Bool) {
  for var c = b; b; b = c {
    if b {
      break
    }
    continue
  }
}
