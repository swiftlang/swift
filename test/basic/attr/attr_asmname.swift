// RUN: %swift %s -parse -verify

@asmname("foo") // expected-note {{attribute already specified here}}
@asmname("bar") // expected-error {{duplicate attribute}}
func duplicateAsmName() {}

// Test parser recovery by having something that
// should parse fine.
func somethingThatShouldParseFine() {}

@!asmname("foo") // expected-error {{attribute may not be inverted}}
func invalidInversion() {}

