// RUN: %swift -typecheck -verify -parse-stdlib -module-name Swift -target x86_64-apple-macosx10.15 %s

@available(OSX, introduced: 10.16)
func longFormIntroducedIn10_16() { }

@available(OSX, introduced: 10.18)
func longFormIntroducedIn10_18() { }

@available(OSX, introduced: 11.0)
func longFormIntroducedIn11_0() { }

@available(OSX, introduced: 13.0)
func longFormIntroducedIn13_0() { }

// expected-note@+1 *{{add '@available' attribute to enclosing global function}}
func useLongFromIntroduced() {
  longFormIntroducedIn10_16()
  // expected-error@-1{{'longFormIntroducedIn10_16()' is only available in macOS 11.0 or newer}}
  // expected-note@-2{{add 'if #available' version check}}

  longFormIntroducedIn10_18()
  // expected-error@-1{{'longFormIntroducedIn10_18()' is only available in macOS 10.18 or newer}}
  // expected-note@-2{{add 'if #available' version check}}

  longFormIntroducedIn11_0()
  // expected-error@-1{{'longFormIntroducedIn11_0()' is only available in macOS 11.0 or newer}}
  // expected-note@-2{{add 'if #available' version check}}

  longFormIntroducedIn13_0()
    // expected-error@-1{{'longFormIntroducedIn13_0()' is only available in macOS 13.0 or newer}}
    // expected-note@-2{{add 'if #available' version check}}
}

@available(OSX 10.16, *)
func shortFormIntroducedIn10_16() { }

@available(OSX 10.18, *)
func shortFormIntroducedIn10_18() { }

@available(OSX 11.0, *)
func shortFormIntroducedIn11_0() { }

@available(OSX 13.0, *)
func shortFormIntroducedIn13_0() { }

// expected-note@+1 *{{add '@available' attribute to enclosing global function}}
func useShortIntroduced() {
  shortFormIntroducedIn10_16()
    // expected-error@-1{{'shortFormIntroducedIn10_16()' is only available in macOS 11.0 or newer}}
    // expected-note@-2{{add 'if #available' version check}}
  shortFormIntroducedIn10_18()
    // expected-error@-1{{'shortFormIntroducedIn10_18()' is only available in macOS 10.18 or newer}}
    // expected-note@-2{{add 'if #available' version check}}
  shortFormIntroducedIn11_0()
    // expected-error@-1{{'shortFormIntroducedIn11_0()' is only available in macOS 11.0 or newer}}
    // expected-note@-2{{add 'if #available' version check}}

  shortFormIntroducedIn13_0()
    // expected-error@-1{{'shortFormIntroducedIn13_0()' is only available in macOS 13.0 or newer}}
    // expected-note@-2{{add 'if #available' version check}}
}
