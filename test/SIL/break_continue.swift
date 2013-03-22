// RUN: %swift -dump-sil %s | FileCheck %s

// CHECK: func_decl test1
func test1(b:Bool) {
  for var c = b; b; b = c {
    if b {
      break
    }
    continue
  }
}
