// RUN: %empty-directory(%t)

/// This should fail when defining 16.0 and using just 16
// RUN: not %target-swift-frontend -typecheck -diagnostic-style llvm \
// RUN:   %s \
// RUN:   -define-availability "MyLib 16: macOS 16" \
// RUN:   -define-availability "MyLib 16.0: macOS 16.0" \
// RUN:   2>&1 | %FileCheck %s

// CHECK: -define-availability argument:1:1: error: duplicate definition of availability macro 'MyLib' for version '16.0'

func test() {
    if #available(MyLib 16, *) {
        print("available 16")
    }

    if #available(MyLib 16.0, *) {
        print("available 16.0")
    }
}
