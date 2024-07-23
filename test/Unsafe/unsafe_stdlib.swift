// RUN: %target-typecheck-verify-swift -enable-experimental-feature AllowUnsafeAttribute -enable-experimental-feature DisallowUnsafe

// Make sure everything compiles without error when unsafe code is allowed.
// RUN: %target-swift-frontend -typecheck -enable-experimental-feature AllowUnsafeAttribute %s

func test(
  x: OpaquePointer, // expected-error{{reference to unsafe struct 'OpaquePointer'}}
  other: UnsafeMutablePointer<Int> // expected-error{{reference to unsafe generic struct 'UnsafeMutablePointer'}}
) {
  var array = [1, 2, 3]
  // expected-error@+1{{call to instance method 'withUnsafeBufferPointer' involves unsafe type 'UnsafeBufferPointer<Int>'}}
  array.withUnsafeBufferPointer{ buffer in // expected-note{{'buffer' declared here}}
    print(buffer) // expected-error{{reference to parameter 'buffer' involves unsafe type 'UnsafeBufferPointer<Int>'}}
  }
  array.append(4)
  _ = array
}
