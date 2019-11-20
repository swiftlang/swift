// RUN: %target-typecheck-verify-swift -enable-invalid-ephemeralness-as-error
// REQUIRES: objc_interop

func unsafePointerInitEphemeralConversions() {
  class C {}
  var c: C?

  // FIXME(rdar://57360581): Once we re-introduce the @_nonEphemeral attribute,
  // this should produce an error.
  _ = AutoreleasingUnsafeMutablePointer(&c)
}
