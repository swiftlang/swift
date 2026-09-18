// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -disable-availability-checking \
// RUN:   -module-name test

// REQUIRES: swift_feature_Lifetimes

// Iterating over a temporary `Iterable`, e.g. `array.span` or the result of a
// getter, must not be diagnosed as an escape. The borrowing iterator is
// lifetime-dependent on the sequence and is used for the entire duration of the
// loop, so the desugared loop binds the sequence to an implicit local whose
// scope encloses the loop.

func sumLet(_ array: [Int]) -> Int {
  var total = 0
  for i in array.span {
    total += i
  }
  return total
}

func sumVar() -> Int {
  var array = [1, 2, 3]
  array.append(4)
  var total = 0
  for i in array.span {
    total += i
  }
  return total
}

func makeArray() -> [Int] { [1, 2, 3] }

// The array temporary that the span depends on must outlive the loop too.
func sumRValueBase() -> Int {
  var total = 0
  for i in makeArray().span {
    total += i
  }
  return total
}

func emptySequence() -> Int {
  let array: [Int] = []
  var count = 0
  for _ in array.span {
    count += 1
  }
  return count
}

func nestedLoops(_ outer: [Int], _ inner: [Int]) -> Int {
  var total = 0
  for x in outer.span {
    for y in inner.span {
      total += x + y
    }
  }
  return total
}

func whereClause(_ array: [Int]) -> [Int] {
  var evens: [Int] = []
  for i in array.span where i.isMultiple(of: 2) {
    evens.append(i)
  }
  return evens
}

func breakAndContinue(_ array: [Int]) -> [Int] {
  var result: [Int] = []
  for i in array.span {
    if i == 0 { continue }
    if i < 0 { break }
    result.append(i)
  }
  return result
}

// Binding the sequence to a local already worked; check that it still does.
func explicitBinding(_ array: [Int]) -> Int {
  let span = array.span
  var total = 0
  for i in span {
    total += i
  }
  return total
}

// A sequence read from a binding that cannot be implicitly copied must not be
// rebound: a 'borrowing' parameter cannot be consumed, and it needs no extra
// scope anyway.
struct NoncopyableInt: ~Copyable {
  var value: Int
}

func borrowingParameter(seq: borrowing Span<NoncopyableInt>) -> Int {
  var total = 0
  for element in seq {
    total += element.value
  }
  return total
}

func borrowingParameterCopyable(seq: borrowing Span<Int>) -> Int {
  var total = 0
  for i in seq {
    total += i
  }
  return total
}

func consumingParameter(seq: consuming Span<Int>) -> Int {
  var total = 0
  for i in seq {
    total += i
  }
  return total
}

func mutableLocal(_ array: [Int]) -> Int {
  var span = array.span
  var total = 0
  for i in span {
    total += i
  }
  span = array.span
  _ = span
  return total
}

// Extending the sequence over the loop must not paper over an exclusivity
// violation: the read access on `array` stays live for the whole loop.
func mutateDuringLoop() {
  var array = [1, 2, 3]
  for i in array.span { // expected-note {{conflicting access is here}}
    array.append(i) // expected-error {{overlapping accesses to 'array', but modification requires exclusive access; consider copying to a local variable}}
  }
}

// Whether a sequence expression produces a temporary cannot be told from its
// shape, so all of these are bound: reading a computed property calls its
// getter and yields a temporary just like a call does, even though the
// expression is an lvalue.

struct NonescapableSequence: ~Escapable, Iterable {
  struct BorrowingIterator: ~Escapable, BorrowingIteratorProtocol {
    @_lifetime(&self)
    mutating func nextSpan(maxCount: Int) throws(Never) -> Span<Int> {
      Span()
    }
  }

  @_lifetime(borrow self)
  func makeBorrowingIterator() -> BorrowingIterator { BorrowingIterator() }
}

var settableComputed: NonescapableSequence {
  get { NonescapableSequence() }
  set {}
}

func fromSettableComputedProperty() {
  for _ in settableComputed {}
}

var readOnlyComputed: NonescapableSequence { NonescapableSequence() }

func fromReadOnlyComputedProperty() {
  for _ in readOnlyComputed {}
}

struct Wrapper {
  var array: [Int] = [1, 2, 3]
}

func fromStoredProperty(_ wrapper: Wrapper) -> Int {
  var total = 0
  for i in wrapper.array.span {
    total += i
  }
  return total
}

func fromForceUnwrap(_ array: [Int]?) -> Int {
  var total = 0
  for i in array!.span {
    total += i
  }
  return total
}

func fromOptionalChain(_ wrapper: Wrapper?) -> Int {
  var total = 0
  for i in (wrapper?.array)!.span {
    total += i
  }
  return total
}

// A noncopyable sequence cannot be copied into the binding, so it is borrowed
// there instead.

struct NoncopyableSequence: ~Copyable, ~Escapable, Iterable {
  struct BorrowingIterator: ~Copyable, ~Escapable, BorrowingIteratorProtocol {
    @_lifetime(&self)
    mutating func nextSpan(maxCount: Int) throws(Never) -> Span<Int> {
      Span()
    }
  }

  @_lifetime(borrow self)
  func makeBorrowingIterator() -> BorrowingIterator { BorrowingIterator() }
}

var noncopyableComputed: NoncopyableSequence { NoncopyableSequence() }

func fromNoncopyableComputedProperty() {
  for _ in noncopyableComputed {}
}

func fromNoncopyableTemporary() {
  for _ in NoncopyableSequence() {}
}

func fromNoncopyableLocal() {
  let seq = NoncopyableSequence()
  for _ in seq {}
}

func fromNoncopyableMutableLocal() {
  var seq = NoncopyableSequence()
  for _ in seq {}
  seq = NoncopyableSequence()
  for _ in seq {}
}

func fromNoncopyableBorrowingParameter(_ seq: borrowing NoncopyableSequence) {
  for _ in seq {}
}

func fromNoncopyableConsumingParameter(_ seq: consuming NoncopyableSequence) {
  for _ in seq {}
}
