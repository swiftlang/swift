// RUN: %swift -typecheck %s -verify -target arm64-apple-ios8.0 -parse-stdlib
// RUN: %swift -typecheck %s -verify -target arm64-apple-tvos9 -parse-stdlib
// RUN: %swift -typecheck %s -verify -target i386-apple-watchos4.0 -parse-stdlib
// RUN: %swift -typecheck %s -verify -target x86_64-apple-macos11.0 -parse-stdlib

#if os(Darwin)
func xDarwinFunc() {}
#else
func someOtherPlatformFunc() {}
#endif

#if !os(Darwin) 
struct SomeNonDarwinType {}
#endif

#if os(Darwin)
xDarwinFunc()
someOtherPlatformFunc() // expected-error {{cannot find 'someOtherPlatformFunc' in scope}}
#endif

#if os(Darwin)
let x = SomeNonDarwinType() // expected-error {{cannot find 'SomeNonDarwinType' in scope}}
#else
func NonDarwinTypeReturningFunc() -> SomeNonDarwinType {
    return SomeNonDarwinType()
}
#endif