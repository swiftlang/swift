// RUN: %target-typecheck-verify-swift -enable-experimental-feature AllowUnsafeAttribute -enable-experimental-feature WarnUnsafe -print-diagnostic-groups

// REQUIRES: swift_feature_AllowUnsafeAttribute
// REQUIRES: swift_feature_WarnUnsafe

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

// expected-warning@+3{{global function 'h' has an interface that involves unsafe types}}
// expected-note@+2{{add '@unsafe' to indicate that this declaration is unsafe to use}}{{1-1=@unsafe }}
// expected-note@+1{{add '@safe' to indicate that this declaration is memory-safe to use}}{{1-1=@safe }}
func h(_: UnsafeType) { // expected-note{{reference to unsafe struct 'UnsafeType'}}
// expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  unsafeFunction() // expected-note{{reference to unsafe global function 'unsafeFunction()'}}

  // okay
  unsafe unsafeFunction()

  // expected-warning@+1{{no unsafe operations occur within 'unsafe' expression}}
  unsafe g()
}

// expected-warning@+3 {{global function 'rethrowing' has an interface that involves unsafe types}}
// expected-note@+2{{add '@unsafe' to indicate that this declaration is unsafe to use}}{{1-1=@unsafe }}
// expected-note@+1{{add '@safe' to indicate that this declaration is memory-safe to use}}{1-1=@safe }}
func rethrowing(body: (UnsafeType) throws -> Void) rethrows { } // expected-note{{reference to unsafe struct 'UnsafeType'}}

class HasStatics {
  // expected-warning@+3{{static method 'f' has an interface that involves unsafe types}}
// expected-note@+2{{add '@unsafe' to indicate that this declaration is unsafe to use}}{{3-3=@unsafe }}
// expected-note@+1{{add '@safe' to indicate that this declaration is memory-safe to use}}{3-3=@safe }}
  static internal func f(_: UnsafeType) { } // expected-note{{reference to unsafe struct 'UnsafeType'}}

  
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
  // expected-note@-1{{reference}}

  for _ in unsafe uas { } // okay
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
  @safe var safeCount: Int { unsafe count }
}

func testMyArray(ints: MyArray<Int>) {
  ints.withUnsafeBufferPointer { buffer in
    let bufferCopy = unsafe buffer
    _ = unsafe bufferCopy

    print(buffer.safeCount)
    unsafe print(buffer.baseAddress!)
  }
}
