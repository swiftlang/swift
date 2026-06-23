// RUN: %target-typecheck-verify-swift

let x = switch Bool.random() { case true: x case false: 0 }
// expected-error@-1 {{use of global variable 'x' before its declaration}}
// expected-note@-2 {{'x' declared here}}
