// RUN: %target-typecheck-verify-swift %clang-importer-sdk -enable-objc-interop

import CoreMIDI

func test() -> String {
  let s: String = MIDIGetNumberOfDevices() // expected-error {{cannot convert value of type 'Int' to specified type 'String'}}
  return s
}
