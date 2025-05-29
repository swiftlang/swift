// RUN: not %target-swift-frontend -enable-experimental-async-top-level -swift-version 6 -typecheck %s %S/Inputs/foo.swift 2>&1 | %FileCheck %s --check-prefixes='Swift6-CHECK,CHECK'
// RUN: not %target-swift-frontend -enable-experimental-async-top-level -swift-version 5 -typecheck %s %S/Inputs/foo.swift 2>&1 | %FileCheck %s --check-prefixes='Swift5-CHECK,CHECK'

var a = 10

@MainActor
var b = 14
// CHECK: top-level code variables cannot have a global actor

func nonIsolatedSynchronous() {
    print(a)
// Swift6-CHECK: main actor-isolated var 'a' can not be referenced from a nonisolated context
// Swift6-CHECK: add '@MainActor' to make global function 'nonIsolatedSynchronous()' part of global actor 'MainActor'

// Swift5-CHECK-NOT: main actor-isolated var 'a' can not be referenced from a nonisolated context
// Swift5-CHECK-NOT: add '@MainActor' to make global function 'nonIsolatedSynchronous()' part of global actor 'MainActor'
}

func nonIsolatedAsync() async {
    print(a)
// CHECK: expression is 'async' but is not marked with 'await'
// CHECK: property access is 'async'
}

await nonIsolatedAsync()

// Swift6-CHECK: foo.swift{{.*}}main actor-isolated var 'a' can not be referenced from a nonisolated context
// Swift6-CHECK-DAG: var declared here
// Swift6-CHECK-DAG: add '@MainActor' to make global function 'foo()' part of global actor 'MainActor'

// Swift5-CHECK-NOT: foo.swift{{.*}}main actor-isolated var 'a' can not be referenced from a nonisolated context
// Swift5-CHECK-NOT: var declared here
// Swift5-CHECK-NOT: add '@MainActor' to make global function 'foo()' part of global actor 'MainActor'

@MainActor
func isolated() {
    print(a)
}

func asyncFun() async {
    await print(a)
}

await asyncFun()
