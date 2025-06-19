// RUN: %swift -typecheck -verify -parse-stdlib -module-name Swift -target %target-cpu-apple-macosx26.0 %s


@available(OSX, introduced: 10.5, deprecated: 10.8, obsoleted: 11.0,
              message: "you don't want to do that anyway")
func obsoletedIn11() { }
// expected-note @-1{{'obsoletedIn11()' was obsoleted in macOS 11.0}}

obsoletedIn11() // expected-error{{'obsoletedIn11()' is unavailable in macOS: you don't want to do that anyway}}


@available(OSX, introduced: 10.5, deprecated: 10.8, obsoleted: 10.16,
              message: "you don't want to do that anyway")
func obsoletedIn10_16() { }
// expected-note @-1{{'obsoletedIn10_16()' was obsoleted in macOS 11.0}}

obsoletedIn10_16() // expected-error{{'obsoletedIn10_16()' is unavailable in macOS: you don't want to do that anyway}}

@available(OSX, introduced: 10.5, deprecated: 10.8, obsoleted: 16.0,
              message: "you don't want to do that anyway")
func obsoletedIn16() { }
// expected-note @-1{{'obsoletedIn16()' was obsoleted in macOS 26.0}}

obsoletedIn16() // expected-error{{'obsoletedIn16()' is unavailable in macOS: you don't want to do that anyway}}

@available(OSX, introduced: 10.5, deprecated: 10.8, obsoleted: 26.0,
              message: "you don't want to do that anyway")
func obsoletedIn26() { }
// expected-note @-1{{'obsoletedIn26()' was obsoleted in macOS 26.0}}

obsoletedIn26() // expected-error{{'obsoletedIn26()' is unavailable in macOS: you don't want to do that anyway}}

@available(OSX, deprecated: 10.16)
func deprecatedIn10_16() { }

@available(OSX, deprecated: 10.18)
func deprecatedIn10_18() { }

@available(OSX, deprecated: 11.0)
func deprecatedIn11_0() { }

@available(OSX, deprecated: 16.0)
func deprecatedIn16_0() { }

@available(OSX, deprecated: 26.0)
func deprecatedIn26_0() { }

@available(OSX, deprecated: 27.0)
func deprecatedIn27_0() { }

@available(OSXApplicationExtension, deprecated: 10.16)
func deprecatedIn10_16AppExtension() { }

func useDeprecated() {
  deprecatedIn10_16() // expected-warning {{deprecatedIn10_16()' was deprecated in macOS 11.0}}
  deprecatedIn10_18() // expected-warning {{'deprecatedIn10_18()' was deprecated in macOS 10.18}}
  deprecatedIn11_0() // expected-warning {{'deprecatedIn11_0()' was deprecated in macOS 11.0}}
  deprecatedIn16_0() // expected-warning {{'deprecatedIn16_0()' was deprecated in macOS 26.0}}
  deprecatedIn26_0() // expected-warning {{'deprecatedIn26_0()' was deprecated in macOS 26.0}}

  deprecatedIn27_0()
  deprecatedIn10_16AppExtension()
}
