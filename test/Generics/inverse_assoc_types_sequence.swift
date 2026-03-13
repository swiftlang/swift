// Ban these words past the first 2 lines to ensure we dont' accidentally rely on the stdlib versions
// RUN: cat %s | tail -n +3 | not grep -E 'Sequence|IteratorProtocol'

// RUN: %target-run-simple-swift(-parse-as-library -Xfrontend -disable-availability-checking -Xfrontend -sil-verify-all -enable-experimental-feature SuppressedAssociatedTypesWithDefaults -enable-experimental-feature Lifetimes) \
// RUN:   | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_SuppressedAssociatedTypesWithDefaults
// REQUIRES: swift_feature_Lifetimes

// UNSUPPORTED: back_deployment_runtime || use_os_stdlib

/// MARK: IterProto
public protocol IterProto<Element>: ~Copyable {
  associatedtype Element: ~Copyable

  mutating func next() -> Element?
}

public protocol BorrowingIterProto<Element>: ~Copyable, ~Escapable {
  associatedtype Element: ~Copyable

  @_lifetime(&self)
  mutating func nextSpan(maximumCount: Int) -> Span<Element>
}

// An iterator that not permit any iteration via IterProto...
// currently via saying it's always empty!
public struct NeverIterator<Element: ~Copyable>: IterProto {
  public mutating func next() -> Element? { return nil }
}

/// A default makeIterator for types using NeverIterator
extension Seq where Iterator == NeverIterator<Element>, Self: ~Copyable, Element: ~Copyable {
  public __consuming func makeIterator() -> Iterator {
    return NeverIterator()
  }
}

public struct OldIteratorAdapter<Iter: IterProto>: BorrowingIterProto where Iter.Element: ~Copyable {
  public typealias Element = Iter.Element

  var iter: Iter
  var buffer: UnsafeMutableBufferPointer<Element>
  var isInitialized = false

  public init(_ old: Iter) {
    self.iter = old
    self.buffer = UnsafeMutableBufferPointer<Element>.allocate(capacity: 1)
  }

  private mutating func tryDeinitBuffer() {
    if isInitialized {
      buffer.deinitialize()
      isInitialized = false
    }
  }

  @_lifetime(&self)
  public mutating func nextSpan(maximumCount: Int) -> Span<Element> {
    if let elm = iter.next() {
      // FIXME: Hack to return a Span<Element> that can leak memory if iteration stops early!
      // Need a `deinit` to clean that last one up, but this struct should remain Copyable.
      tryDeinitBuffer()
      buffer.initializeElement(at: 0, to: elm)
      isInitialized = true
      return buffer.span
    }
    tryDeinitBuffer()
    return Span()
  }
}


/// MARK: Seq
public protocol Seq<Element>: ~Copyable {
	associatedtype Element: ~Copyable 

  // NOTE: Iterator remains Copyable
	associatedtype Iterator: IterProto
	  where Iterator.Element == Element

  __consuming func makeIterator() -> Iterator

  // The ***new hotness***
  associatedtype BorrowingIter: BorrowingIterProto = OldIteratorAdapter<Iterator>
  	  where BorrowingIter.Element == Element,
  	        BorrowingIter: ~Copyable,
  	        BorrowingIter: ~Escapable

  @_lifetime(borrow self)
  borrowing func makeBorrowingIter() -> BorrowingIter

  // Less interesting parts of Seq are below

  @available(*, unavailable, renamed: "Iterator")
  typealias Generator = Iterator

//   var underestimatedCount: Int { get }

//   func _customContainsEquatableElement(
//     _ element: borrowing Element    // new: added 'borrowing'
//   ) -> Bool?

// FIXME: how can these be provided as requirements for noncopyable elements
// other than to consume the elements, rather than copy?
//
//   __consuming func _copyToContiguousArray() -> ContiguousArray<Element>
//
//   __consuming func _copyContents(
//     initializing ptr: UnsafeMutableBufferPointer<Element>
//   ) -> (Iterator,UnsafeMutableBufferPointer<Element>.Index)


//   @safe
//   func withContiguousStorageIfAvailable<R>(
//     _ body: (_ buffer: UnsafeBufferPointer<Element>) throws -> R
//   ) rethrows -> R?
}

// FIXME: the below extension runs into a problem when trying to get Array: Seq,
// due to the type checker not being able to decide which `underestimatedCount`
// to use as a witness. There might be an underlying issue to be resolved?
//
// note: multiple matching properties named 'underestimatedCount' with type 'Int
//
//     88 | extension Seq where Self: ~Copyable, Self.Element: ~Copyable {
//     89 |   public var underestimatedCount: Int {
//        |              `- note: candidate exactly matches [with Element = Array<Element>.Element, BorrowingIter = OldIteratorAdapter<Array<Element>.Iterator>]
//
//
//      1 | protocol Se@uence {
//      2 | @inlinable public var underestimatedCount: Int { get }}
//        |                       `- note: candidate exactly matches [with Element = Array<Element>.Element, BorrowingIter = OldIteratorAdapter<Array<Element>.Iterator>]
//
//
// Provide defaults for some of the less common requirements,
// regardless of the kind of Seq.
// extension Seq where Self: ~Copyable, Self.Element: ~Copyable {
//   public var underestimatedCount: Int {
//     return 0
//   }
//
//   public func _customContainsEquatableElement(
//     _ element: borrowing Iterator.Element  // new: added 'borrowing'
//   ) -> Bool? {
//     return nil
//   }
//
//   @safe
//   public func withContiguousStorageIfAvailable<R>(
//     _ body: (UnsafeBufferPointer<Element>) throws -> R
//   ) rethrows -> R? {
//     return nil
//   }
// }


// Provides a default associated type witness for Iterator when the
// Self type is both a Seq and an IterProto.
extension Seq where Self: IterProto, Self: ~Copyable, Self.Element: ~Copyable {
  @_implements(Seq, Iterator)
  public typealias _Default_Iterator = Self
}

/// A default makeIterator() function for `IterProto` instances that
/// are declared to conform to `Seq` where Self: Copyable
extension Seq where Self.Iterator == Self, Self.Element: ~Copyable {
  /// Returns an iterator over the elements of this seq.
  public __consuming func makeIterator() -> Self {
    return self
  }
}

// FIXME: There's a few protocol design issues here in providing the following extension...
// 1. Seq requires Escapable, so it cannot be == Self.BorrowingIter, which is ~Escapable, so
//    we can't provide this convenience unless `Seq` doesn't require Escapable too.
//
// 2. When trying to provide this when Self: Escapable, we get an error about copying Self when
//    returning. We need a borrowed return version of this method to avoid that copy!
//
// extension Seq where Self.BorrowingIter == Self, Self: ~Copyable, Self.Element: ~Copyable {
//   /// Returns a borrowing iterator over the elements of this seq.
//   borrowing func makeBorrowingIter() -> Self {
//     return self
//   }
// }

// We can provide a `makeBorrowingIter` if they're using `OldIteratorAdapter`
extension Seq where BorrowingIter == OldIteratorAdapter<Iterator> {
  public borrowing func makeBorrowingIter() -> BorrowingIter {
    let s = copy self
    return BorrowingIter(s.makeIterator())
  }
}

/// MARK: method iteration

extension Seq where Self: ~Copyable, Self.Element: ~Copyable {
  borrowing func forborrow(_ f: (borrowing Self.Element) -> Void) {
    var iter = makeBorrowingIter()
     while true {
      let span = iter.nextSpan(maximumCount: 32)
      if span.count <= 0 { break }
      for i in 0..<span.count {
        f(span[i])
      }
    }
  }

  consuming func forconsume(_ f: (consuming Self.Element) -> Void) {
    var iter = makeIterator()
    while let elm = iter.next() {
      f(elm)
    }
  }
}

// Simple foreach method.
extension Seq {
  func foreach<Elm>(_ seq: some Seq<Elm>, _ f: (Elm) -> Void) {
    var iter = seq.makeIterator()
    while let elm = iter.next() {
      f(elm)
    }
  }
}

/// MARK: freestanding iteration
// mostly for demonstration, as the method ones are more ergonomic.

func foreach2<Elm>(_ seq: some Seq<Elm>, _ f: (Elm) -> Void) {
  var iter = seq.makeIterator()
  while let elm = iter.next() {
    f(elm)
  }
}

func forconsume2<S, E>(_ seq: S, _ f: (consuming E) -> Void)
 where S: Seq, S.Element == E, E: ~Copyable {
  var iter = seq.makeIterator()
  while let elm = iter.next() {
    f(elm)
  }
}

func forborrow2<S, E>(_ seq: borrowing S, _ f: (borrowing E) -> Void)
  where S: Seq, S: ~Copyable, E: ~Copyable, S.Element == E {
  var iter = seq.makeBorrowingIter()
   while true {
    let span = iter.nextSpan(maximumCount: 32)
    if span.count <= 0 { break }
    for i in 0..<span.count {
      f(span[i])
    }
  }
}


/// MARK: conforming types

extension IndexingIterator: IterProto {}
extension Array: Seq {}


extension Dictionary.Iterator: IterProto {}
extension Dictionary: Seq {}

public struct SpanIterator<Element: ~Copyable>: BorrowingIterProto, ~Escapable {
  let span: Span<Element>
  var didProvide = false

  @_lifetime(copy sp)
  init(_ sp: Span<Element>) {
    self.span = sp
  }

  @_lifetime(&self)
  public mutating func nextSpan(maximumCount: Int) -> Span<Element> {
    if didProvide { return Span() }
    didProvide = true
    return span
  }
}

extension InlineArray: Seq where Element: ~Copyable {
  public typealias Iterator = NeverIterator<Element>
  public typealias BorrowingIter = SpanIterator<Element>

  @_lifetime(borrow self)
  public borrowing func makeBorrowingIter() -> BorrowingIter {
    return SpanIterator(self.span)
  }
}

/// MARK: execution testing

struct NC<T>: ~Copyable {
  let val: T
  init(_ t: T) { self.val = t }
  deinit { print("destroying \(val)") }
}

@main struct Main {
  static func main() {
    print("=== InlineArray forborrow test ===")
    testInlineArrayForborrow()

    print("=== Array forborrow test ===")
    testArrayForborrow()

    print("\n=== Array forconsume test ===")
    testArrayForconsume()

    print("\n=== Dictionary forborrow test ===")
    testDictionaryForborrow()

    print("\n=== Dictionary forconsume test ===")
    testDictionaryForconsume()
  }

  // MARK: InlineArray tests

    static func testInlineArrayForborrow() {
        let numbers: [_ of NC<Int>] = [NC(1), NC(2), NC(3), NC(4), NC(5)]
        var sum = 0

        // CHECK: InlineArray forborrow: 1
        // CHECK: InlineArray forborrow: 2
        // CHECK: InlineArray forborrow: 3
        // CHECK: InlineArray forborrow: 4
        // CHECK: InlineArray forborrow: 5
        numbers.forborrow { nc in
          sum += nc.val
          print("InlineArray forborrow: \(nc.val)")
        }

        // CHECK: InlineArray forborrow sum: 15
        print("InlineArray forborrow sum: \(sum)")
        assert(sum == 15, "Array forborrow sum should be 15")

        // CHECK: destroying 1
        // CHECK: destroying 2
        // CHECK: destroying 3
        // CHECK: destroying 4
        // CHECK: destroying 5
      }

  // MARK: - Array Tests

  static func testArrayForborrow() {
    let numbers = [1, 2, 3, 4, 5]
    var sum = 0

    // CHECK: forborrow: 1
    // CHECK: forborrow: 2
    // CHECK: forborrow: 3
    // CHECK: forborrow: 4
    // CHECK: forborrow: 5
    numbers.forborrow { num in
      sum += num
      print("forborrow: \(num)")
    }

    print("Array forborrow sum: \(sum)")
    assert(sum == 15, "Array forborrow sum should be 15")
  }

  static func testArrayForconsume() {
    let strings = ["alpha", "beta", "gamma"]
    var collected: [String] = []

    // CHECK: forconsume: alpha
    // CHECK: forconsume: beta
    // CHECK: forconsume: gamma
    strings.forconsume { str in
      collected.append(str.uppercased())
      print("forconsume: \(str)")
    }

    // CHECK: Collected: ["ALPHA", "BETA", "GAMMA"]
    print("Collected: \(collected)")
  }

  // MARK: - Dictionary Tests

  static func testDictionaryForborrow() {
    let dict: [String: Int] = ["a": 10, "b": 20, "c": 30]
    var product = 1

    // CHECK: forborrow: a = 10
    // CHECK: forborrow: b = 20
    // CHECK: forborrow: c = 30
    dict.forborrow { pair in
      let (key, value) = pair
      product *= value
      print("forborrow: \(key) = \(value)")
    }

    print("Product of values: \(product)")
    assert(product == 6000, "Dictionary forborrow product should be 6000")
  }

  static func testDictionaryForconsume() {
    let dict: [String: String] = ["greeting": "hello", "farewell": "goodbye"]
    var count = 0

    // CHECK: forconsume: greeting -> hello
    // CHECK: forconsume: farewell -> goodbye
    dict.forconsume { pair in
      let (key, value) = pair
      count += 1
      print("forconsume: \(key) -> \(value)")
    }

    print("Dictionary pair count: \(count)")
    assert(count == 2, "Dictionary forconsume should process 2 pairs")
  }
}
