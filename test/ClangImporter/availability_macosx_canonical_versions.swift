// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -I %S/Inputs/custom-modules -target %target-cpu-apple-macosx10.15 %s

// REQUIRES: OS=macosx

import MacOSVersionCanonicalization

FunctionIntroducedIn10_16()
// expected-error@-1 {{'FunctionIntroducedIn10_16()' is only available in macOS 11.0 or newer}}
// expected-note@-2 {{add 'if #available' version check}}

FunctionIntroducedIn11_0()
// expected-error@-1 {{'FunctionIntroducedIn11_0()' is only available in macOS 11.0 or newer}}
// expected-note@-2 {{add 'if #available' version check}}

FunctionIntroducedIn16_0()
// expected-error@-1 {{'FunctionIntroducedIn16_0()' is only available in macOS 26.0 or newer}}
// expected-note@-2 {{add 'if #available' version check}}

FunctionIntroducedIn26_0()
// expected-error@-1 {{'FunctionIntroducedIn26_0()' is only available in macOS 26.0 or newer}}
// expected-note@-2 {{add 'if #available' version check}}
