// RUN: %batch-code-completion

// https://github.com/apple/swift/issues/80591

// Just make sure we don't crash.
var foo: Bool {
  baz == .#^COMPLETE^#
  // COMPLETE: Begin completions
}
