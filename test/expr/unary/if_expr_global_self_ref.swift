// RUN: %target-typecheck-verify-swift

let x = if .random() { x } else { 0 }
// expected-error@-1 {{use of global variable 'x' before its declaration}}
// expected-note@-2 {{'x' declared here}}
