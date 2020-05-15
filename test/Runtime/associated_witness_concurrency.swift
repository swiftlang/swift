// RUN: %target-swiftc_driver %s -g %import-libdispatch -o %t
// RUN: %target-codesign %t
// RUN: %target-run %t
// REQUIRES: libdispatch
// REQUIRES: executable_test

import Dispatch

protocol P {
  associatedtype Unused
}
struct A<T> : P {
  // We never actually read this associated type, but its presence in the
  // wtable should force it to be instantiated rather than shared for
  // all specializations of A.
  typealias Unused = A<A<T>>
}

protocol Q {
  associatedtype Assoc: P

  func foo()
}
struct B<T: P> : Q {
  // Both the metadata and the wtable for this associated type require
  // runtime instantiation and must be fetched lazily.
  typealias Assoc = A<T>

  func foo() {}
}

// It's possible that the optimizer might someday be able to recognize
// that this is a no-op.
func rundown<T: Q>(value: T, count: Int) {
  guard count > 0 else { return }

  value.foo()

  // Assuming that T is B<U> for some U, this constructs B<A<U>>,
  // which will be T in the recursive call; i.e. we should have a
  // different metadata/wtable pair each time.  In order to construct
  // that, we have to read the associated metadata and wtable for T.Assoc.
  rundown(value: B<T.Assoc>(), count: count - 1)
}

DispatchQueue.concurrentPerform(iterations: 5) { n in
  rundown(value: B<A<()>>(), count: 1000)
}
