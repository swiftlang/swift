// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -I %S/Inputs/custom-modules -target x86_64-apple-macosx10.15 %s

// REQUIRES: OS=macosx

import MacOSVersionCanonicalization

FunctionIntroducedIn10_16()
// expected-error@-1 {{'FunctionIntroducedIn10_16()' is only available in macOS 11.0 or newer}}
// expected-note@-2 {{add 'if #available' version check}}

FunctionIntroducedIn11_0()
// expected-error@-1 {{'FunctionIntroducedIn11_0()' is only available in macOS 11.0 or newer}}
// expected-note@-2 {{add 'if #available' version check}}
