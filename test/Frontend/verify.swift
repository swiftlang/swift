// Tests for the Swift frontends `-verify` mode.

// RUN: not %target-typecheck-verify-swift 2>&1 | %FileCheck %s

// CHECK: [[@LINE+1]]:1: error: unexpected error produced: cannot find 'undefinedFunc' in scope
undefinedFunc()

// CHECK: [[@LINE+1]]:4: error: expected error not produced
// expected-error{{This error message never gets output}}

anotherUndefinedFunc()
// CHECK: [[@LINE+1]]:13: error: expected warning, not error
// expected-warning@-2 {{cannot find 'anotherUndefinedFunc' in scope}}

// CHECK: [[@LINE+1]]:20: error: expected {{{{}} in {{expected}}-{{warning}}
// expected-warning

func fn() {}
fn(())    // expected-error {{argument passed to call that takes no arguments}}
fn(())    // expected-error {{argument passed to call that takes no arguments}} {{4-6=}}
fn(())    // expected-error {{argument passed to call that takes no arguments}} {{4-6=}}||{{3-4=fnord}}
fn(())    // expected-error {{argument passed to call that takes no arguments}} {{3-4=fnord}} || {{4-6=}}
fn(())    // expected-error {{argument passed to call that takes no arguments}} {{4-6=}} {{none}}
fn(())    // expected-error {{argument passed to call that takes no arguments}} {{4-6=}}||{{3-4=fnord}} {{none}}
fn(())    // expected-error {{argument passed to call that takes no arguments}} {{3-4=fnord}}||{{4-6=}} {{none}}

// CHECK: [[@LINE+1]]:81: error: expected fix-it not seen; actual fix-it seen: {{[{][{]4-6=[}][}]}}
fn(())    // expected-error {{argument passed to call that takes no arguments}} {{3-4=fnord}} {{4-6=}}

// CHECK: [[@LINE+1]]:81: error: expected no fix-its; actual fix-it seen: {{[{][{]4-6=[}][}]}}
fn(())    // expected-error {{argument passed to call that takes no arguments}} {{none}}

// CHECK: [[@LINE+2]]:8: error: unexpected error produced: generic type 'Array' specialized with too many type parameters
// CHECK: note: diagnostic produced elsewhere: generic type 'Array' declared here
let x: Array<Int, Int>
