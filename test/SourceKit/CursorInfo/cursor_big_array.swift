// RUN: %sourcekitd-test -req=cursor -pos=1:10 %S/../Inputs/big_array.swift -- %S/../Inputs/big_array.swift | %FileCheck %s
// CHECK: source.lang.swift.decl.var.global (1:5-1:20)
// CHECK: gCubeVertexData
// CHECK: s:v9big_array15gCubeVertexDataGSaSf_
// CHECK: [Float]
