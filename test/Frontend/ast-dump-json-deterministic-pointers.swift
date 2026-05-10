// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target %target-swift-5.9-abi-triple -swift-version 6 -parse-as-library -dump-ast -dump-ast-format json %s -module-name main -o - > %t/main.json
// RUN: %{python} -c 'import json, sys; print(json.dumps(json.load(sys.stdin), indent=4))' < %t/main.json | %FileCheck %s

// CHECK: "_kind": "func_decl",
// CHECK: "decl_context": "replaced-pointer-[[FILE_ID:[0-9]+]]",
// CHECK: "name": "f"
func f() {
    // CHECK: "_kind": "pattern_binding_decl",
    // CHECK: "decl_context": "replaced-pointer-[[F_ID:[0-9]+]]",
    // CHECK: "name": "x"
    // CHECK: "_kind": "var_decl",
    // CHECK: "decl_context": "replaced-pointer-[[F_ID]]",
    var x = 0

    // CHECK: "_kind": "pattern_binding_decl",
    // CHECK: "decl_context": "replaced-pointer-[[F_ID:[0-9]+]]",
    // CHECK: "name": "y"
    // CHECK: "_kind": "var_decl",
    // CHECK: "decl_context": "replaced-pointer-[[F_ID]]",
    var y = 0
}

// CHECK: "_kind": "func_decl",
// CHECK: "decl_context": "replaced-pointer-[[FILE_ID]]",
// CHECK: "name": "g"
func g() {
    // CHECK: "_kind": "pattern_binding_decl",
    // CHECK: "decl_context": "replaced-pointer-[[G_ID:[0-9]+]]",
    // CHECK: "name": "z"
    // CHECK: "_kind": "var_decl",
    // CHECK: "decl_context": "replaced-pointer-[[G_ID]]",
    var z = 0
}
