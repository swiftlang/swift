// RUN: %target-run-simple-swift(-enable-experimental-feature Lifetimes \
// RUN: -Xfrontend -disable-availability-checking) \
// RUN: %s | %FileCheck %s

// RUN: %target-run-simple-swift(-O -enable-experimental-feature Lifetimes \
// RUN: -Xfrontend -disable-availability-checking) \
// RUN: %s | %FileCheck %s

// REQUIRES: swift_feature_Lifetimes
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// XFAIL: swift_test_mode_optimize_none_with_opaque_values

// The borrowing iterator of a for-each loop depends on the sequence, which the
// loop borrows for its whole duration. The iterator is destroyed at the end of
// the loop, and its deinit may still read the sequence's storage, so the
// sequence must not be released before the iterator is destroyed.
//
// The sequence here holds strings that are too large to be stored inline, so
// reading them after the sequence has been released reads freed memory.

func makeStorage() -> [2 of String] {
  [String(repeating: "x", count: 40), String(repeating: "y", count: 40)]
}

struct Seq: Iterable {
  struct BorrowingIterator: ~Copyable, ~Escapable, BorrowingIteratorProtocol {
    var span: Span<String>
    var pos = 0

    @_lifetime(copy span)
    init(span: Span<String>) { self.span = span }

    @_lifetime(&self)
    mutating func nextSpan(maxCount: Int) throws(Never) -> Span<String> {
      if pos > 0 { return Span() }
      pos = 1
      return span
    }

    deinit {
      let intact = span.count == 2
        && span[0] == String(repeating: "x", count: 40)
        && span[1] == String(repeating: "y", count: 40)
      print("iterator deinit: \(intact ? "ok" : "corrupted")")
    }
  }

  var storage: [2 of String]

  @_lifetime(borrow self)
  func makeBorrowingIterator() -> BorrowingIterator {
    BorrowingIterator(span: storage.span)
  }
}

@inline(never)
func makeSeq() -> Seq { Seq(storage: makeStorage()) }

// The sequence is a temporary that only the loop keeps alive.
@inline(never)
func iterateTemporary() {
  for x in makeSeq() { print(x.count) }
}

// The sequence is a named local.
@inline(never)
func iterateNamed() {
  let seq = makeSeq()
  for x in seq { print(x.count) }
}

// Leaving the loop early must also destroy the iterator first.
@inline(never)
func iterateWithBreak() {
  for x in makeSeq() {
    print(x.count)
    break
  }
}

// CHECK: temporary
// CHECK-NEXT: 40
// CHECK-NEXT: 40
// CHECK-NEXT: iterator deinit: ok
print("temporary")
iterateTemporary()

// CHECK-NEXT: named
// CHECK-NEXT: 40
// CHECK-NEXT: 40
// CHECK-NEXT: iterator deinit: ok
print("named")
iterateNamed()

// CHECK-NEXT: break
// CHECK-NEXT: 40
// CHECK-NEXT: iterator deinit: ok
print("break")
iterateWithBreak()
