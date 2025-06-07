// RUN: %sourcekitd-test -req=complete -pos=45:1 %S/../Inputs/big_array.swift -- %S/../Inputs/big_array.swift | %FileCheck %s
// CHECK: key.kind: source.lang.swift.decl.var.global
// CHECK: key.name: "gCubeVertexData"
// CHECK: key.description: "gCubeVertexData"
// CHECK: key.typename: "[Float]"
// CHECK: key.sourcetext: "gCubeVertexData"
