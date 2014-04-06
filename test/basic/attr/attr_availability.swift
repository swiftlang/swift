// RUN: %swift %s -parse -verify

@availability(*, unavailable)
func unavailable_func() {}

@availability(*, unavailable, message="message")
func unavailable_func_with_message() {}

@availability(ios, unavailable)
@availability(macosx, unavailable)
func unavailable_multiple_platforms() {}

@availability(badPlatform, unavailable) // expected-error {{unknown platform 'badPlatform' for attribute 'availability'}} expected-error {{expected declaration}}
func unavailable_bad_platform() {}

@!availability(*, unavailable) // expected-error {{attribute may not be inverted}}
func availabilityInvalidInversion() {}

