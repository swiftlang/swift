// RUN: %target-swift-frontend -enable-experimental-feature SwiftSettings -enable-experimental-feature Macros -c -swift-version 6 -disable-availability-checking -verify -Xllvm -swift-settings-allow-duplicates %s

// REQUIRES: asserts
// REQUIRES: concurrency
// REQUIRES: swift_feature_Macros
// REQUIRES: swift_feature_SwiftSettings

actor MyActor {}

#SwiftSettings(.defaultIsolation(MainActor.self))
#SwiftSettings(.defaultIsolation(nil))
#SwiftSettings(.defaultIsolation(MyActor.self)) // expected-error {{Unrecognized setting passed to #SwiftSettings}}
#SwiftSettings(.defaultIsolation(1)) // expected-error {{Unrecognized setting passed to #SwiftSettings}}
// expected-error @-1 {{cannot convert value of type 'Int' to expected argument type 'any Actor.Type'}}
#SwiftSettings(2) // expected-error {{Unrecognized setting passed to #SwiftSettings}}
// expected-error @-1 {{cannot convert value of type 'Int' to expected argument type 'SwiftSetting'}}

#SwiftSettings(.defaultIsolation(MainActor.self),
               .defaultIsolation(nil))

