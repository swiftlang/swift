// RUN: %swift -typecheck %s -verify -target arm64-apple-ios8.0

// REQUIRES: OS=ios

#if os(Darwin)
func xDarwinFunc() {}
#else
func someOtherPlatformFunc() {}
#endif

xDarwinFunc()
someOtherPlatformFunc() // expected-error {{cannot find 'someOtherPlatformFunc' in scope}}