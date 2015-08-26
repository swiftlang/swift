// RUN: %target-parse-verify-swift %clang-importer-sdk

// REQUIRES: objc_interop

import CoreMIDI

func test() -> String {
  let s: String = MIDIGetNumberOfDevices() // expected-error {{cannot convert call result type 'Int' to expected type 'String'}}
  return s
}
