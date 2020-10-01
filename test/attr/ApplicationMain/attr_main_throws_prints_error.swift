// RUN: %empty-directory(%t)
// RUN: %target-build-swift -parse-as-library %s -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main > %t/log 2>&1 || true
// RUN: %FileCheck %s < %t/log

// REQUIRES: executable_test
// REQUIRES: OS=macosx || OS=ios

enum Err : Error { case or }

// CHECK: Fatal error: Error raised at top level: main.Err.or:
@main
struct S {
    static func main() throws {
        throw Err.or
    }
}
