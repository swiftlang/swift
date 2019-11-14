// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -swift-version 4 -o %t/main
// RUN: not --crash %target-run %t/main
// REQUIRES: asserts, executable_test

// TF-427: Segfault when `@differentiable` function is stored in a struct/class
// that also has a generic property.

// On Ubuntu: crash occurs only with `-swift-version 4`.
// On macOS: crash occurs with `-target x86_64-apple-darwin15` but not
// `-target x86_64-apple-darwin14`.

struct Wrapper<T> {
  var t: T
  var f: @differentiable (Float) -> Float = { $0 }
  init(_ t: T) { self.t = t }
}
let w = Wrapper(1)

// Segmentation fault.
