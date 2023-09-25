// RUN: %target-typecheck-verify-swift -enable-experimental-feature NoncopyableGenerics

// REQUIRES: asserts

protocol Eq: ~Copyable {
  func same(_ other: Self) -> Bool
}

struct File: ~Copyable, Eq {
  let fd: Int = 0
  deinit {}

  func same(_ other: borrowing Self) -> Bool {
    return fd == other.fd
  }
}

// FIXME: missing ownership is not diagnosed!
func check<T: ~Copyable>(_ a: T, _ b: T) -> Bool where T: Eq {
    return a.same(b)
}
