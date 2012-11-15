// RUN: %swift -dump-sil %s | FileCheck %s

class Ref {
}
struct Val {
}

// CHECK: func_decl local_reftype
func local_reftype() {
    var a = new Ref()
    // CHECK: alloc_var a
    // CHECK: destroy
    // CHECK: dealloc
}

// CHECK: func_decl local_valtype
func local_valtype() {
    var b = Val()
    // CHECK: alloc_var b
    // CHECK: destroy
    // CHECK: dealloc
}
