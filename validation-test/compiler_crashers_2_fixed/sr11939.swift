// RUN: %target-swift-frontend %s -typecheck -verify

func sr_11939(_ closure: @autoclosure () -> String...) {} // expected-error {{'@autoclosure' must not be used on variadic parameters}}
sr_11939("A") // No crash
