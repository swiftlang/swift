// RUN: %target-typecheck-verify-swift -enable-experimental-feature WarnUnsafe

// Make sure everything compiles without error when unsafe code is allowed.
// RUN: %target-swift-frontend -typecheck -enable-experimental-feature AllowUnsafeAttribute -warnings-as-errors %s

// REQUIRES: swift_feature_AllowUnsafeAttribute
// REQUIRES: swift_feature_WarnUnsafe

// expected-warning@+1{{global function 'test' involves unsafe code; use '@unsafe' to indicate that its use is not memory-safe}}{{1-1=@unsafe }}
func test(
  x: OpaquePointer, // expected-note{{reference to unsafe struct 'OpaquePointer'}}
  other: UnsafeMutablePointer<Int> // expected-note{{reference to unsafe generic struct 'UnsafeMutablePointer'}}
) {
  var array = [1, 2, 3]
  // expected-note@+1{{call to instance method 'withUnsafeBufferPointer' involves unsafe type 'UnsafeBufferPointer<Int>'}}
  array.withUnsafeBufferPointer{ buffer in
    print(buffer) // expected-note{{reference to parameter 'buffer' involves unsafe type 'UnsafeBufferPointer<Int>'}}
  }
  array.append(4)
  _ = array
}
