// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o
// RUN: %target-clang -x c -std=c11 -c %S/Inputs/debug-malloc.c -o %t/debug-malloc.o
// RUN: %target-clang %t/a.o %t/debug-malloc.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

var x: UInt = 1
func randomNumber() -> UInt {
    x = ((x >> 16) ^ x) &* 0x45d9f3b
    x = ((x >> 16) ^ x) &* 0x45d9f3b
    x = (x >> 16) ^ x
    return x
}

@main
struct Main {
    static func main() {
        for _ in 0 ..< 3 {
            _ = randomNumber().description
        }
        print("OK!")
        // CHECK: malloc
        // CHECK: free
        // CHECK: malloc
        // CHECK: free
        // CHECK: malloc
        // CHECK: free
        // CHECK: OK!
    }
}
