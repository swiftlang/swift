// RUN: %target-typecheck-verify-swift -strict-memory-safety

// Make sure everything compiles without error when unsafe code is allowed.
// RUN: %target-swift-frontend -typecheck -warnings-as-errors %s

func test(
  x: OpaquePointer,
  other: UnsafeMutablePointer<Int>
) {
  var array = [1, 2, 3]
  array.withUnsafeBufferPointer{ buffer in
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}{{5-5=unsafe }}
    print(buffer) // expected-note{{reference to parameter 'buffer' involves unsafe type 'UnsafeBufferPointer<Int>'}}
  }
  array.append(4)
  _ = array
}
