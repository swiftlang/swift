// RUN: %target-typecheck-verify-swift -swift-version 4

func signedBitPattern() {
  _ = Int64(bitPattern: 0.0) // expected-error {{Please use Int64(bitPattern: UInt64) in combination with Double.bitPattern property.}}
  _ = Int32(bitPattern: 0.0) // expected-error {{Please use Int32(bitPattern: UInt32) in combination with Float.bitPattern property.}}
}
