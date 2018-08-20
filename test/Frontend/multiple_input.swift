// RUN: %target-swift-frontend -typecheck %S/Inputs/multiple_input/main.swift %S/Inputs/multiple_input/lib.swift %s -module-name ThisModule
// RUN: %target-swift-frontend -typecheck %s %S/Inputs/multiple_input/lib.swift %S/Inputs/multiple_input/main.swift -module-name ThisModule

// RUN: not %target-swift-frontend -typecheck -parse-as-library %S/Inputs/multiple_input/main.swift %S/Inputs/multiple_input/lib.swift %s 2>&1 | %FileCheck %s
// CHECK: main.swift:
// CHECK: expressions are not allowed at the top level
