// rdar://176562065
// RUN: %target-typecheck-verify-swift

func getName() -> UnsafePointer<CChar> {
  var value = "unknown"
  return value.withUTF8 { $0 } // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
