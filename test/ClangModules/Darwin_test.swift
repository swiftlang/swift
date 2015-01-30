// RUN: %target-parse-verify-swift %clang-importer-sdk

// REQUIRES: objc_interop

import Darwin

let _: Fract? = nil // expected-error{{use of undeclared type 'Fract'}}
let _: Darwin.Fract? = nil // okay

let _: OSErr = 0
let _: OSStatus = noErr // noErr is from the overlay
let _: UniChar = 0

let _ = ProcessSerialNumber()

let _: Byte = 0 // expected-error {{use of undeclared type 'Byte'}}
Darwin.fakeAPIUsingByteInDarwin() as Int // expected-error {{'UInt8' is not convertible to 'Int'}}
