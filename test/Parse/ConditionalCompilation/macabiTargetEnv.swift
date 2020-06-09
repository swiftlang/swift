// RUN: %swift -swift-version 4 -typecheck %s -verify -target x86_64-apple-ios12.0-macabi -parse-stdlib
// RUN: %swift-ide-test -swift-version 4 -test-input-complete -source-filename=%s -target x86_64-apple-ios12.0-macabi

// REQUIRES: OS=maccatalyst

#if targetEnvironment(macabi) // expected-warning {{'macabi' has been renamed to 'macCatalyst'}} {{23-29=macCatalyst}}
func underMacABI() {
  foo() // expected-error {{cannot find 'foo' in scope}}
}
#endif

#if !targetEnvironment(macabi) // expected-warning {{'macabi' has been renamed to 'macCatalyst'}} {{24-30=macCatalyst}}
// This block does not typecheck but the #if prevents it from
// from being a compiler error.
let i: SomeType = "SomeString" // no-error
#endif

#if targetEnvironment(macCatalyst)
func underTargetEnvironmentMacCatalyst() {
  foo() // expected-error {{cannot find 'foo' in scope}}
}
#endif

// Make sure we don't treat the macabi environment as a simulator.
#if targetEnvironment(simulator)
// This block does not typecheck but the #if prevents it from
// from being a compiler error.
let i: SomeType = "SomeString" // no-error
#endif

#if os(macCatalyst)
// expected-warning@-1 {{unknown operating system for build configuration 'os'}}
// expected-note@-2 *{{did you mean}}
func underOSMacCatalyst() {
}
#endif
