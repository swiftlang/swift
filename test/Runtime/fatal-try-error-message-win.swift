// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s %S/Inputs/setup-handler-win.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: not %target-run %t/a.out 2>&1 | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: OS=windows-msvc

// CHECK: Fatal error message: a/fatal-try-error-message-win.swift:{{[0-9]+}}: Fatal error: 'try!' expression unexpectedly raised an error: a.Errs.getMyDescription("Try crash!")

enum Errs: Error {
    case getMyDescription(String)
}

func throwings(_ i: Int) throws {
    if i == 0 {
        throw Errs.getMyDescription("Try crash!")
    }
}

func crash() {
    try! throwings(0)
}

@main
struct Test {
    static func main() throws {
        setupHandler()
        crash()
    }
}
