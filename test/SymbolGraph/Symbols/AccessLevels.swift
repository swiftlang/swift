// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name AccessLevels -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name AccessLevels -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/AccessLevels.symbols.json

// CHECK: "accessLevel": "public" 

public struct PublicStruct {
  public var x: Int
}

// CHECK-NOT: "accessLevel": "private" 
