// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -disable-requirement-machine-concrete-contraction

// This triggered a Requirement Machine assertion because the permanent
// 'identity conformance' rule ([P].[P] => [P]) was the source of the
// conflict.

struct S {}

// expected-error@+1 {{same-type requirement makes generic parameter 'Self' non-generic}}
protocol P where Self == S {}
