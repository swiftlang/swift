// REQUIRES: swift_feature_ProtocolMetatypeExtensions
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/MetatypeExtLib.swiftmodule -module-name MetatypeExtLib -enable-experimental-feature ProtocolMetatypeExtensions %S/Inputs/metatype-extension-lib.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-feature ProtocolMetatypeExtensions -I %t

import MetatypeExtLib

// Members from the metatype extension should be accessible on the protocol metatype.
let _: Int = P.value

// Members from the metatype extension should NOT be accessible on conforming types.
struct S: P {}
let _ = S.value // expected-error {{type 'S' has no member 'value'}}
