// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/issue-85134
// RUN: %target-codesign %t/issue-85134
// RUN: %target-run %t/issue-85134

// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: executable_test

// UNSUPPORTED: back_deployment_runtime

@globalActor
final actor FooActor { // IMPORTANT: **final** actor
    static let shared = FooActor()
}

protocol P {}
struct Foo: @FooActor P {}

func checkConformance() {
    let foo = Foo()
    let _ = foo is P // 💥 Runtime crash: EXC_BAD_ACCESS (code=1, address=0x0)
}

checkConformance()
