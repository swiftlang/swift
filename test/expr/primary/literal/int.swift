// RUN: %target-typecheck-verify-swift

do {
  func takeInt32(_: Int32) {}

  func f1(_ cint: CInt) {
    takeInt32(UInt(cint))
    // expected-error@-1:15 {{cannot convert value of type 'UInt' to expected argument type 'Int32'}}{{15-15=Int32(}}{{25-25=)}}
  }
  func f2(_ uint: UInt) {
    takeInt32(uint)
    // expected-error@-1:15 {{cannot convert value of type 'UInt' to expected argument type 'Int32'}}{{15-15=Int32(}}{{19-19=)}}

    let int32: Int32 = uint
    // expected-error@-1:24 {{cannot convert value of type 'UInt' to specified type 'Int32'}}{{24-24=Int32(}}{{28-28=)}}
  }
}

