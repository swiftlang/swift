// RUN: %swift -dump-sil %s | FileCheck %s

class Ref {
}
struct Val {
}

// CHECK: func_decl local_reftype
func local_reftype() {
    var a = new Ref()
    // CHECK: alloc_var a
    // CHECK: release a
}

// CHECK: func_decl local_valtype
func local_valtype() {
    var b = new Val()
    // CHECK: alloc_var b
    // CHECK: release b
}
