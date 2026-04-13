// RUN: %target-typecheck-verify-swift

protocol P {}

extension P.Protocol {} // expected-error {{protocol metatype extensions require '-enable-experimental-feature ProtocolMetatypeExtensions'}}
