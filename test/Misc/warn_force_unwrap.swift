// RUN: %target-swift-frontend -warn-force-unwrap -typecheck -verify -c %s

var a: String?

print(a!) // expected-warning {{consider using an alternative to a forced unwrap}}

