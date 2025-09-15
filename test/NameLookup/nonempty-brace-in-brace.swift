// RUN: not %target-swift-frontend -typecheck %s  2>&1 |  %FileCheck %s --check-prefix=CHECK-NO-ASSERTION

// CHECK-NO-ASSERTION-NOT: Assertion
// Used to trip an assertion


public struct Foo {
    func bar() {
        var copySelf = self
        repeat { copySelf

private extension String {}

// Note: extra newlines below ensure that context printing doesn't show the
// lines that we shouldn't see.



// EOF
