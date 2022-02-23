// RUN: %target-typecheck-verify-swift -requirement-machine-protocol-signatures=on

// This triggered a Requirement Machine assertion because the permanent
// 'identity conformance' rule ([P].[P] => [P]) was the source of the
// conflict.

struct S {}

protocol P where Self == S {}
