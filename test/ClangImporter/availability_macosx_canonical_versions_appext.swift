// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -I %S/Inputs/custom-modules -target %target-cpu-apple-macosx10.15 -application-extension %s

// REQUIRES: OS=macosx

import MacOSVersionCanonicalization

FunctionIntroducedIn10_16AppExt()
// expected-error@-1 {{'FunctionIntroducedIn10_16AppExt()' is only available in application extensions for macOS 11.0 or newer}}
// expected-note@-2 {{add 'if #available' version check}}

FunctionIntroducedIn11_0AppExt()
// expected-error@-1 {{'FunctionIntroducedIn11_0AppExt()' is only available in application extensions for macOS 11.0 or newer}}
// expected-note@-2 {{add 'if #available' version check}}

FunctionIntroducedIn16_0AppExt()
// expected-error@-1 {{'FunctionIntroducedIn16_0AppExt()' is only available in application extensions for macOS 26.0 or newer}}
// expected-note@-2 {{add 'if #available' version check}}

FunctionIntroducedIn26_0AppExt()
// expected-error@-1 {{'FunctionIntroducedIn26_0AppExt()' is only available in application extensions for macOS 26.0 or newer}}
// expected-note@-2 {{add 'if #available' version check}}
