// RUN: %target-typecheck-verify-swift -strict-memory-safety

// Make sure everything compiles without error when unsafe code is allowed.
// RUN: %target-swift-frontend -typecheck -warnings-as-errors %s

func test(
  x: OpaquePointer,
  other: UnsafeMutablePointer<Int>
) {
  var array = [1, 2, 3]
  // expected-warning@+3{{expression uses unsafe constructs but is not marked with 'unsafe'}}{{3-3=unsafe }}
  // expected-note@+2{{argument #0 in call to instance method 'withUnsafeBufferPointer' has unsafe type '(UnsafeBufferPointer<Int>) -> ()'}}
  // expected-note@+1{{reference to instance method 'withUnsafeBufferPointer' involves unsafe type 'UnsafeBufferPointer<Int>'}}
  array.withUnsafeBufferPointer{ buffer in
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}{{5-5=unsafe }}
    print(buffer) // expected-note{{reference to parameter 'buffer' involves unsafe type 'UnsafeBufferPointer<Int>'}}
  }
  array.append(4)
  _ = array
}
