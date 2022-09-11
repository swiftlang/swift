// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/47003
// '#if' in top-level code that contains only declarations should not disturb
// forward reference.

typealias A = B

#if false
func foo() {}
#endif

struct B {}

typealias C = D

#if true
print("ok")
#endif

struct D {}
