// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

// Check that string literals that are unicode scalar literals 
// are emitted as string_literal instead of integers.
func zzz() -> String {
    return "1"+"2"
}
// CHECK: string_literal utf8 "1"
// CHECK: string_literal utf8 "2"
