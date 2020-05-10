// RUN: %empty-directory(%t)
// RUN: %target-build-swift -parse-as-library %s -o %t/main
// RUN: %target-codesign %t/main
// RUN: not --crash %t/main 2>&1 | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: OS=macosx

enum Err : Error { case or }

// CHECK: Fatal error: Error raised at top level: main.Err.or:
@main
struct S {
    static func main() throws {
        throw Err.or
    }
}
