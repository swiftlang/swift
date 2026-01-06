// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// rdar://130168101: Make sure that we correctly resolve types in
// the generic context of a protocol extension with a `Self` same
// type constraint.


protocol P { }

struct P2: P { }

extension P where Self == P2 {
    @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
    dynamic func p2() -> some P {
        return self
    }
}

// CHECK: P2()
if #available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *) {
    print(P2().p2())
} else {
    print(P2())
}
