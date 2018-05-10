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

// CHECK: "leadingTrivia":[
// CHECK: "kind":"LineComment",
// CHECK: "value":"\/\/ Comment at the end of the file"

// Comment at the end of the file
