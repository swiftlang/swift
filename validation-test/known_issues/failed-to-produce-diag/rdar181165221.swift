// rdar://181165221
// RUN: %target-typecheck-verify-swift

let twoBytes: (UInt8, UInt8) = (0, 0)
let n = withUnsafeBytes(of: twoBytes) { buf in
  // expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
  String(cString: buf.baseAddress!)
}
