// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name FunctionSignature -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name FunctionSignature -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/FunctionSignature.symbols.json

public func foo(_ noext: Int, ext int: Int) -> String {
  return "OK"
}

// CHECK: "name": "noext"
// CHECK-NOT: "internalName": "noext"
// CHECK-NEXT: declarationFragments

// CHECK: "kind": "identifier"
// CHECK-NEXT: "spelling": "noext" 
// CHECK: "kind": "text"
// CHECK-NEXT: "spelling": ": " 
// CHECK: "kind": "typeIdentifier"
// CHECK-NEXT: "spelling": "Int" 
// CHECK-NEXT: "preciseIdentifier": "s:Si" 

// CHECK: "name": "ext"
// CHECK-NEXT: "internalName": "int"
// CHECK-NEXT: declarationFragments

// CHECK: "kind": "identifier"
// CHECK-NEXT: "spelling": "int" 
// CHECK: "kind": "text"
// CHECK-NEXT: "spelling": ": " 
// CHECK: "kind": "typeIdentifier"
// CHECK-NEXT: "spelling": "Int" 
// CHECK-NEXT: "preciseIdentifier": "s:Si" 

// CHECK: returns
// CHECK: "kind": "typeIdentifier" 
// CHECK-NEXT: "spelling": "String" 
// CHECK-NEXT: "preciseIdentifier": "s:SS"
