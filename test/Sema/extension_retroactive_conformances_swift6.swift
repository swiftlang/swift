// RUN: %target-typecheck-verify-swift -swift-version 6

struct LocalStruct {}
protocol LocalProtocol {}

extension LocalStruct: @retroactive LocalProtocol {} // expected-error {{'retroactive' attribute does not apply; 'LocalStruct' is declared in this module}}

extension Int: @retroactive LocalProtocol {} // expected-error {{'retroactive' attribute does not apply; 'LocalProtocol' is declared in this module}}
