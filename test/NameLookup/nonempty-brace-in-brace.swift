// RUN: not %target-swift-frontend -typecheck %s  2>&1 |  %FileCheck %s --check-prefix=CHECK-NO-ASSERTION

// Used to trip an assertion

public struct Foo {
    func bar() {
        var copySelf = self
        repeat { copySelf

private extension String {}

// CHECK-NO-ASSERTION-NOT: Assertion
