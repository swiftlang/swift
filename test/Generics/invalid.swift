// RUN: %target-parse-verify-swift

func bet() where A : B {} // expected-error {{'where' clause cannot be attached to a non-generic declaration}}

typealias gimel where A : B // expected-error {{'where' clause cannot be attached to a non-generic declaration}}
// expected-error@-1 {{expected '=' in typealias declaration}}

class dalet where A : B {} // expected-error {{'where' clause cannot be attached to a non-generic declaration}}

protocol he where A : B { // expected-error {{'where' clause cannot be attached to a protocol declaration}}

  associatedtype vav where A : B // expected-error {{'where' clause cannot be attached to an associated type declaration}}
}
