// RUN: %swift -parse %S/Inputs/main.swift %S/Inputs/empty.swift %s
// RUN: %swift -parse %s %S/Inputs/empty.swift %S/Inputs/main.swift

// RUN: not %swift -parse -parse-as-library %S/Inputs/main.swift %S/Inputs/empty.swift %s 2>&1 | FileCheck %s
// CHECK: main.swift:
// CHECK: expressions are not allowed at the top level

func someLibraryFunction() {}

var someGlobal = 42 + 20
