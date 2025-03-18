// RUN: %target-swift-frontend -enable-experimental-feature SwiftSettings -enable-experimental-feature Macros -c -swift-version 6 -disable-availability-checking -verify %s

// REQUIRES: asserts
// REQUIRES: concurrency
// REQUIRES: swift_feature_Macros
// REQUIRES: swift_feature_SwiftSettings

// This test specifically tests the behavior of #SwiftSettings when we find
// multiple instances of the same setting.

actor MyActor {}

#SwiftSettings(.defaultIsolation(MainActor.self), // expected-note 6 {{setting originally passed here}}
               .defaultIsolation(nil)) // expected-error {{duplicate setting passed to #SwiftSettings}}
#SwiftSettings(.defaultIsolation(nil)) // expected-error {{duplicate setting passed to #SwiftSettings}}
#SwiftSettings(.defaultIsolation(MyActor.self)) // expected-error {{duplicate setting passed to #SwiftSettings}}
#SwiftSettings(.defaultIsolation(1)) // expected-error {{duplicate setting passed to #SwiftSettings}}
// expected-error @-1 {{cannot convert value of type 'Int' to expected argument type 'any Actor.Type'}}
#SwiftSettings(2) // expected-error {{Unrecognized setting passed to #SwiftSettings}}
// expected-error @-1 {{cannot convert value of type 'Int' to expected argument type 'SwiftSetting'}}

#SwiftSettings(.defaultIsolation(MainActor.self), // expected-error {{duplicate setting passed to #SwiftSettings}}
               .defaultIsolation(nil)) // expected-error {{duplicate setting passed to #SwiftSettings}}

