// RUN: %target-parse-verify-swift

@availability(*, unavailable)
func unavailable_func() {}

@availability(*, unavailable, message="message")
func unavailable_func_with_message() {}

@availability(iOS, unavailable)
@availability(OSX, unavailable)
func unavailable_multiple_platforms() {}

@availability // expected-error {{expected '(' in 'availability' attribute}}
func noArgs() {}
@availability(*) // expected-error {{expected ',' in 'availability' attribute}}
func noKind() {}

@availability(badPlatform, unavailable) // expected-error {{unknown platform 'badPlatform' for attribute 'availability'}}
func unavailable_bad_platform() {}

// Handle unknown platform.
@availability(HAL9000, unavailable) // expected-error {{unknown platform 'HAL9000'}}
func availabilityUnknownPlatform() {}

// <rdar://problem/17669805> Availability can't appear on a typealias
@availability(*, unavailable, message="oh no you dont")
typealias int = Int // expected-note {{'int' has been explicitly marked unavailable here}}

@availability(*, unavailable, renamed="Float")
typealias float = Float // expected-note {{'float' has been explicitly marked unavailable here}}


var x : int // expected-error {{'int' is unavailable: oh no you dont}}
var y : float // expected-error {{'float' has been renamed to Float}}{{9-14=Float}}


// More complicated parsing.
@availability(OSX, message="x", unavailable)
let _: Int;

@availability(OSX, introduced=1, deprecated=2.0, obsoleted=3.0.0)
let _: Int

@availability(OSX, introduced=1.0.0, deprecated=2.0, obsoleted=3, unavailable, renamed="x")
let _: Int

// Meaningless but accepted.
@availability(OSX, message="x")
let _: Int;


// Parse errors.
@availability() // expected-error{{expected platform name or '*' for 'availability' attribute}}
let _: Int

@availability(OSX,) // expected-error{{expected 'availability' option such as 'unavailable', 'introduced', 'deprecated', 'obsoleted', 'message', or 'renamed'}}
let _: Int

@availability(OSX, message) // expected-error{{expected '=' after 'message' in 'availability' attribute}}
let _: Int

@availability(OSX, message=) // expected-error{{expected string literal in 'availability' attribute}} expected-error{{prefix/postfix '=' is reserved}}
let _: Int

@availability(OSX, message=x) // expected-error{{expected string literal in 'availability' attribute}}
let _: Int

@availability(OSX, unavailable=) // expected-error{{expected ')' in 'availability' attribute}} expected-error{{prefix/postfix '=' is reserved}} expected-error{{expected declaration}}
let _: Int

@availability(OSX, introduced) // expected-error{{expected '=' after 'introduced' in 'availability' attribute}}
let _: Int

@availability(OSX, introduced=) // expected-error{{expected version number in 'availability' attribute}} expected-error{{prefix/postfix '=' is reserved}}
let _: Int

@availability(OSX, introduced=x) // expected-error{{expected version number in 'availability' attribute}}
let _: Int

@availability(OSX, introduced=1.x) // expected-error{{expected ')' in 'availability' attribute}} expected-error {{expected declaration}}
let _: Int

@availability(OSX, introduced=1.0.x) // expected-error{{expected version number in 'availability' attribute}}
let _: Int

@availability(OSX, introduced=0x1) // expected-error{{expected version number in 'availability' attribute}}
let _: Int

@availability(OSX, introduced=1.0e4) // expected-error{{expected version number in 'availability' attribute}}
let _: Int

@availability(OSX, introduced=-1) // expected-error{{expected '=' after 'introduced' in 'availability' attribute}} expected-error{{expected declaration}}
let _: Int

@availability(OSX, introduced=1.0.1e4) // expected-error{{expected version number in 'availability' attribute}}
let _: Int

@availability(OSX, introduced=1.0.0x4) // expected-error{{expected version number in 'availability' attribute}}
let _: Int
