// RUN: %target-typecheck-verify-swift

func foo(_ a: [[UInt8]], _ p: [UnsafeRawPointer]) {
  foo(a, a) // expect-warning {{all paths through this function will call itself}}
}

// rdar://problem/44658089
func takesPtr(_: UnsafePointer<UInt8>) {}

func givesPtr(_ str: String) {
  // FIXME(rdar://57360581): Once we re-introduce the @_nonEphemeral attribute,
  // this should produce a warning.
  takesPtr(UnsafePointer(str))
}
