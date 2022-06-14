// RUN: %swift -target arm64-apple-ios8.0 -disable-legacy-type-info -parse-stdlib -enable-objc-interop -disable-objc-attr-requires-foundation-module -I %S/Inputs/usr/include -emit-ir %s -o - | %FileCheck %s -check-prefix CHECK-MACHO

// REQUIRES: OS=ios

#if os(Darwin)
func xDarwinFunc() {

}
#else
func someOtherPlatformFunc() {}
#endif

xDarwinFunc()
someOtherPlatformFunc() // expected-error {{cannot find 'someOtherPlatformFunc' in scope}}