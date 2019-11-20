// RUN: %target-typecheck-verify-swift
// REQUIRES: objc_interop

func unsafePointerInitEphemeralConversions() {
  class C {}
  var c: C?

  // FIXME(rdar://57360581): Once we re-introduce the @_nonEphemeral attribute,
  // this should produce a warning.
  _ = AutoreleasingUnsafeMutablePointer(&c)
}
