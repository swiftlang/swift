// rdar://176562065
// RUN: %target-typecheck-verify-swift

func getName() -> UnsafePointer<CChar> {
  var value = "unknown"
  return value.withUTF8 { $0 } // expected-error {{cannot convert return expression of type 'UnsafeBufferPointer<UInt8>' to return type 'UnsafePointer<CChar>' (aka 'UnsafePointer<Int8>')}}
}
