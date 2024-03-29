// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s %S/Inputs/setup-handler-win.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: not %target-run %t/a.out 2>&1 | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: OS=windows-msvc

// CHECK: Fatal error message: a/fatal-error-message-win.swift:{{[0-9]+}}: Fatal error: Fatal crash!

func crash() {
    fatalError("Fatal crash!")
}

@main
struct Test {
    static func main() throws {
        setupHandler()
        crash()
    }
}
