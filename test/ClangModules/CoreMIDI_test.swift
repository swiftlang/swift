// RUN: %target-parse-verify-swift %clang-importer-sdk

// REQUIRES: objc_interop

import CoreMIDI

func test() {
  let _: String = MIDIGetNumberOfDevices() // expected-error {{'Int' is not convertible to 'String'}}
}
