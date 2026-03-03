// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: not %target-swift-frontend-verify -typecheck %t/test.swift 2>&1 | %update-verify-tests
// RUN: %target-swift-frontend-verify -typecheck %t/test.swift
// RUN: %diff %t/test.swift %t/test.swift.expected

//--- test.swift
func foo() {
         //     expected-error@+1    2{{cannot find 'a' in scope}}
    a = 2; a = 2; b = 2; b = 2; c = 2;
         //     expected-error@+1    2{{asdf}}
    d = 2;
    e = 2; f = 2;                 //     expected-error    2{{cannot find 'e' in scope}}
}

//--- test.swift.expected
func foo() {
         //     expected-error@+3    {{cannot find 'c' in scope}}
         //     expected-error@+2    2{{cannot find 'b' in scope}}
         //     expected-error@+1    2{{cannot find 'a' in scope}}
    a = 2; a = 2; b = 2; b = 2; c = 2;
         //     expected-error@+1    {{cannot find 'd' in scope}}
    d = 2;
    //     expected-error@+1    {{cannot find 'f' in scope}}
    e = 2; f = 2;                 //     expected-error    {{cannot find 'e' in scope}}
}

