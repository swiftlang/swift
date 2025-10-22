// RUN: %target-typecheck-verify-swift -strict-concurrency=complete -enable-experimental-feature SendingArgsAndResults

// REQUIRES: concurrency
// REQUIRES: swift_feature_SendingArgsAndResults

// README: This test makes sure that we error when sending is placed in the
// wrong place with respect to ownership modifiers.

class NonSendable {
	init(){}
}

func test_good(_ x: sending NonSendable) {}

func test_consuming_after_sending(_ x: sending consuming NonSendable) {} // expected-error {{'sending' must be placed after specifier 'consuming'}}

func test_inout_after_sending(_ x: sending inout NonSendable) {} // expected-error {{'sending' must be placed after specifier 'inout'}}

func test_repeated_sending(_ x: sending sending NonSendable) {} // expected-error {{parameter may have at most one 'sending' specifier}}

func test_repeated_sending_mixed(_ x: sending consuming sending inout NonSendable) {}
// expected-error @-1 {{'sending' must be placed after specifier 'consuming'}}
// expected-error @-2 {{parameter may have at most one 'sending' specifier}}
// expected-error @-3 {{parameter may have at most one of the 'inout', 'borrowing', or 'consuming' specifiers}}

// Just until we get the results setup.
func test_sending_result_in_tuple() -> (sending NonSendable, NonSendable) {}
// expected-error @-1 {{'sending' cannot be applied to tuple elements}}
