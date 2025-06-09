// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -disable-objc-attr-requires-foundation-module

// REQUIRES: OS=macosx

func markUsed<T>(_ t: T) {}

@available(OSX 10.16, *)
func introducedOn10_16() { }

func useUnderPoundAvailable() {
  if #available(OSX 10.16, *) {
    introducedOn10_16() // no-error
  }
}
