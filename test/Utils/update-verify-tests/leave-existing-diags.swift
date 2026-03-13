// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: not %target-swift-frontend-verify -typecheck %t/test.swift 2>&1 | %update-verify-tests
// RUN: %target-swift-frontend-verify -typecheck %t/test.swift
// RUN: %diff %t/test.swift %t/test.swift.expected

//--- test.swift
func foo() {
    a = 2;
    // expected-error@-1{{cannot find 'a' in scope}}
    b = 2;// expected-error{{cannot find 'b' in scope}}
    c = 2;
    // expected-error@5{{cannot find 'c' in scope}}
    d = 2; // expected-error{{'d' in scope}}

    e = 2; // error to trigger mismatch
}

//--- test.swift.expected
func foo() {
    a = 2;
    // expected-error@-1{{cannot find 'a' in scope}}
    b = 2;// expected-error{{cannot find 'b' in scope}}
    c = 2;
    // expected-error@5{{cannot find 'c' in scope}}
    d = 2; // expected-error{{'d' in scope}}

    // expected-error@+1{{cannot find 'e' in scope}}
    e = 2; // error to trigger mismatch
}

