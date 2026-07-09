// RUN: %target-swiftc_driver -Osize -emit-sil %s -o /dev/null

// https://github.com/swiftlang/swift/issues/90119
//
// With -Osize and cross-module optimization, two distinct [weak self] closures
// stored in escaping-typed locals and passed to a function that iterates a
// Dictionary and calls both closures produced SIL where the alloc_stacks for
// the promoted weak-capture boxes were never deallocated before the return.
//
// SIL verification failed: return with stack allocs that haven't been
// deallocated

class C {
  func trigger(_ dict: [Int: Int]) {
    let f: () -> Void = { [weak self] in _ = self }
    let g: () -> Void = { [weak self] in _ = self }
    callee(dict: dict, f: f, g: g)
  }
}

func callee(dict: [Int: Int], f: () -> Void, g: () -> Void) {
  for v in dict.keys { _ = v }
  f(); g()
}
