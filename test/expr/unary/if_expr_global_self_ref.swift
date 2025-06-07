// RUN: %target-typecheck-verify-swift

let x = if .random() { x } else { 0 }
// expected-error@-1 {{cannot reference invalid declaration 'x'}}
// expected-note@-2 {{'x' declared here}}
