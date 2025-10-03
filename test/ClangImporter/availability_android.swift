// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -I %S/Inputs/custom-modules -verify-ignore-unknown -target aarch64-unknown-linux-android24 %s

// REQUIRES: OS=linux-android ||  OS=linux-androideabi

import AndroidVersioning

FunctionIntroducedIn24()

FunctionIntroducedIn28()
// expected-error@-1 {{'FunctionIntroducedIn28()' is only available in Android 28 or newer}}
// expected-note@-2 {{add 'if #available' version check}}
