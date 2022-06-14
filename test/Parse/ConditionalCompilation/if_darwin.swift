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
let someNum = 3
#endif

xDarwinFunc()
someOtherPlatformFunc() // expected-error {{cannot find 'someOtherPlatformFunc' in scope}}

func anotherFunc() -> Int {
    return someNum // expected-error {{cannot find 'someNum' in scope}}
}