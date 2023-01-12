// RUN: %target-typecheck-verify-swift -enable-experimental-feature StatementExpressions

let x = switch Bool.random() { case true: x case false: 0 }
// expected-error@-1 {{cannot reference invalid declaration 'x'}}
// expected-note@-2 {{'x' declared here}}
