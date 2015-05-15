// RUN: %target-parse-verify-swift %clang-importer-sdk

// REQUIRES: objc_interop

import Darwin

_ = nil as Fract? // expected-error{{use of undeclared type 'Fract'}}
_ = nil as Darwin.Fract? // okay

_ = 0 as OSErr
_ = noErr as OSStatus // noErr is from the overlay
_ = 0 as UniChar

_ = ProcessSerialNumber()

_ = 0 as Byte // expected-error {{use of undeclared type 'Byte'}}
Darwin.fakeAPIUsingByteInDarwin() as Int // expected-error {{'UInt8' is not convertible to 'Int'}}
