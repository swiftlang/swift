// RUN: %target-swift-emit-silgen  -parse-as-library %s -verify | %FileCheck %s
// RUN: %target-swift-emit-ir  -parse-as-library %s

func function1() {
  return

  class UnreachableClass {} // expected-warning {{code after 'return' will never be executed}}
}

func function2() {
  let _ = [
    {
      struct S {
        var x = 0
      }
    }
  ]
}

// CHECK-LABEL: sil private [transparent] [ossa] @$s11local_types9function2yyFyycfU_1SL_V1xSivpfi : $@convention(thin) () -> Int

// CHECK-LABEL: sil_vtable UnreachableClass
