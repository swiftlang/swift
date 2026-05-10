// RUN: %target-typecheck-verify-swift

protocol EmptyProtocol {}
struct EmptyStruct {}

// https://github.com/apple/swift/issues/43674
// Coercion in single expression closure with invalid signature caused segfault
do {
  struct G<T: EmptyProtocol> {}

  let _ = { (_: G<EmptyStruct>) -> Void in // expected-error{{type 'EmptyStruct' does not conform to protocol 'EmptyProtocol'}}
    EmptyStruct() as EmptyStruct
  }
}
