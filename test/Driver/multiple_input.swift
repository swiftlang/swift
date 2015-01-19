// RUN: %target-swift-frontend -parse %S/Inputs/main.swift %S/Inputs/lib.swift %s -module-name ThisModule
// RUN: %target-swift-frontend -parse %s %S/Inputs/lib.swift %S/Inputs/main.swift -module-name ThisModule

// RUN: not %target-swift-frontend -parse -parse-as-library %S/Inputs/main.swift %S/Inputs/lib.swift %s 2>&1 | FileCheck %s
// CHECK: main.swift:
// CHECK: expressions are not allowed at the top level
