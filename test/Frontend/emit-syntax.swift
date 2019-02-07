// RUN: %target-swift-frontend -emit-syntax %s | %FileCheck %s

// CHECK: "kind":"kw_struct"
// CHECK: "kind":"identifier",
// CHECK: "text":"Foo"
// CHECK: "kind":"l_brace"
struct Foo {
  // CHECK: "kind":"kw_let"
  // CHECK: "kind":"colon"
  // CHECK: "kind":"identifier"
  // CHECK: "text":"Int"
  let x: Int
// CHECK: "kind":"r_brace"
}

// CHECK: "kind":"kw_func"
// CHECK: "kind":"identifier"
// CHECK: "text":"Bar"
// CHECK: "kind":"l_paren"
// CHECK: "kind":"FunctionParameterList"
// CHECK: "kind":"FunctionParameter"
// CHECK: "kind":"identifier"
// CHECK: "text":"arg1"
// CHECK: "kind":"colon"
// CHECK: "kind":"identifier"
// CHECK: "text":"String"
// CHECK: null,null
// CHECK: "kind":"comma"
// CHECK: "kind":"identifier"
// CHECK: "text":"arg2"
// CHECK: "kind":"colon"
// CHECK: "kind":"identifier"
// CHECK: "text":"Int"
// CHECK: "kind":"r_paren"
// CHECK: "kind":"l_brace"
func Bar(arg1: String, arg2: Int) {
// CHECK: "kind":"r_brace"
}

// CHECK: "kind":"kw_func"
// CHECK: "text":"CheckParameterList"
// CHECK: "kind":"l_paren"
// CHECK: "text":"arg1"
// CHECK: "text":"String"
// CHECK: "kind":"ellipsis"
// CHECK: "presence":"Present"}
// CHECK: null
// CHECK: "kind":"comma"
// CHECK: "text":"arg2"
// CHECK: "text":"Int"
// CHECK: "kind":"r_paren"
// CHECK: "kind":"l_brace"
func CheckParameterList(arg1: String..., arg2: Int) {
// CHECK: "kind":"r_brace"
}

// CHECK: "leadingTrivia":[
// CHECK: "kind":"LineComment",
// CHECK: "value":"\/\/ Comment at the end of the file"

// Comment at the end of the file
