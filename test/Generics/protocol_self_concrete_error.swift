// RUN: %target-typecheck-verify-swift -requirement-machine-protocol-signatures=on
// RUN: %target-typecheck-verify-swift -requirement-machine-protocol-signatures=on -disable-requirement-machine-concrete-contraction

// This triggered a Requirement Machine assertion because the permanent
// 'identity conformance' rule ([P].[P] => [P]) was the source of the
// conflict.

struct S {}

// expected-error@+1 {{no type for 'Self' can satisfy both 'Self : P' and 'Self == S'}}
protocol P where Self == S {}
