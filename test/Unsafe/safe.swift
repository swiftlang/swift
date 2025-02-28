// RUN: %target-typecheck-verify-swift -strict-memory-safety -print-diagnostic-groups

// The feature flag should be enabled.
#if !hasFeature(StrictMemorySafety)
#error("Strict memory safety is not enabled!")
#endif


@unsafe
func unsafeFunction() { }

@unsafe
struct UnsafeType { }

func f() {
  unsafe unsafeFunction()
}

func g() {
  unsafe unsafeFunction()
}

func h(_: UnsafeType) {
// expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  unsafeFunction() // expected-note{{reference to unsafe global function 'unsafeFunction()'}}

  // okay
  unsafe unsafeFunction()

  // expected-warning@+1{{no unsafe operations occur within 'unsafe' expression}}
  unsafe g()
}

func rethrowing(body: (UnsafeType) throws -> Void) rethrows { }

class HasStatics {
  static internal func f(_: UnsafeType) { }

  
}

@unsafe
func unsafeInt() -> Int { 5 }

struct HasProperties {
  var computed: Int {
    unsafe unsafeInt()
  }

  @unsafe var computedUnsafe: Int {
    unsafe unsafeInt()
  }

  static var blah: Int = {
    unsafe unsafeInt()
  }()

  @unsafe static var blahUnsafe: Int = {
    unsafe unsafeInt()
  }()
}

protocol P { }

extension Int: @unsafe P { }

func acceptP(_: some P) { }

func testConformance(i: Int) {
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  acceptP(i) // expected-note{{@unsafe conformance of 'Int' to protocol 'P' involves unsafe code}}
}

func returnsOpaqueP() -> some P {
  5 // expected-warning{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1{{@unsafe conformance of 'Int' to protocol 'P' involves unsafe code}}
}

func returnsExistentialP() -> any P {
  5 // expected-warning{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1{{@unsafe conformance of 'Int' to protocol 'P' involves unsafe code}}
}

// FIXME: Should work even if the IteratorProtocol conformance is safe
struct UnsafeAsSequence: @unsafe Sequence, @unsafe IteratorProtocol {
  @unsafe mutating func next() -> Int? { nil }
}

func testUnsafeAsSequenceForEach() {
  let uas = UnsafeAsSequence()

  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}{{12-12=unsafe }}
  for _ in uas { } // expected-note{{conformance}}
  // expected-warning@-1{{for-in loop uses unsafe constructs but is not marked with 'unsafe' [StrictMemorySafety]}}{{7-7=unsafe }}

  for _ in unsafe uas { } // expected-warning{{for-in loop uses unsafe constructs but is not marked with 'unsafe' [StrictMemorySafety]}}{{7-7=unsafe }}

  for unsafe _ in unsafe uas { } // okay
}

func testForInUnsafeAmbiguity(_ integers: [Int]) {
  for unsafe in integers {
    _ = unsafe
  }
  for unsafe: Int in integers {
    _ = unsafe
  }
}

struct UnsafeIterator: @unsafe IteratorProtocol {
  @unsafe mutating func next() -> Int? { nil }
}

struct SequenceWithUnsafeIterator: Sequence {
  func makeIterator() -> UnsafeIterator { UnsafeIterator() }
}

func testUnsafeIteratorForEach() {
  let swui = SequenceWithUnsafeIterator()

  for _ in swui { } // expected-warning{{for-in loop uses unsafe constructs but is not marked with 'unsafe'}}{{7-7=unsafe }}
  for unsafe _ in swui { } // okay, it's only the iterator that's unsafe
}

class MyRange {
  @unsafe init(unchecked bounds: Range<Int>) { }

  convenience init(_ bounds: Range<Int>) {
    // bounds check
    self.init(unchecked: bounds) // expected-warning{{expression uses unsafe constructs but is not marked with 'unsafe'}}
    // expected-note@-1{{reference to unsafe initializer 'init(unchecked:)'}}
  }
}

func casting(value: Any, i: Int) {
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = value as? UnsafeType // expected-note{{reference to unsafe type 'UnsafeType'}}
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = value as! UnsafeType // expected-note{{reference to unsafe type 'UnsafeType'}}

  _ = unsafe value as? UnsafeType
  _ = unsafe value as! UnsafeType

  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = i as any P // expected-note{{@unsafe conformance of 'Int' to protocol 'P' involves unsafe code}}
}

func metatypes() {
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  let _: Any.Type = UnsafeType.self // expected-note{{reference to unsafe type 'UnsafeType'}}

  let _: Any.Type = unsafe UnsafeType.self
}

func testKeyPath() {
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = \HasProperties.computedUnsafe // expected-note{{reference to unsafe property 'computedUnsafe'}}

  _ = unsafe \HasProperties.computedUnsafe
}

func takesAutoclosure<T>(_ body: @autoclosure () -> T) { }

func testAutoclosure() {
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}{{3-3=unsafe }}
  takesAutoclosure(unsafeFunction()) // expected-note{{reference to unsafe global function 'unsafeFunction()'}}

  unsafe takesAutoclosure(unsafeFunction())

  takesAutoclosure(unsafe unsafeFunction())
}

// Parsing of `unsafe` expressions.
func testUnsafePositionError() -> Int {
  return 3 + unsafe unsafeInt() // expected-error{{'unsafe' cannot appear to the right of a non-assignment operator}}
}

// @safe suppresses unsafe-type-related diagnostics on an entity
struct MyArray<Element> {
  @safe func withUnsafeBufferPointer<R, E>(
    _ body: (UnsafeBufferPointer<Element>) throws(E) -> R
  ) throws(E) -> R {
    return unsafe try body(.init(start: nil, count: 0))
  }
}

extension UnsafeBufferPointer {
  @unsafe var unsafeCount: Int { 17 }
  @safe var safeCount: Int { unsafe unsafeCount }
}

func testMyArray(ints: MyArray<Int>) {
  ints.withUnsafeBufferPointer { buffer in
    let bufferCopy = unsafe buffer
    _ = unsafe bufferCopy

    print(buffer.safeCount)
    unsafe print(buffer.unsafeCount)
  }
}
