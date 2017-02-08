// RUN: %target-typecheck-verify-swift

// SR-1062: 
// Coercion in single expression closure with invalid signature caused segfault
protocol SR_1062_EmptyProtocol {}

struct SR_1062_EmptyStruct {}
struct SR_1062_GenericStruct<T: SR_1062_EmptyProtocol> {}

let _ = { (_: SR_1062_GenericStruct<SR_1062_EmptyStruct>) -> Void in // expected-error{{type 'SR_1062_EmptyStruct' does not conform to protocol 'SR_1062_EmptyProtocol'}}
  SR_1062_EmptyStruct() as SR_1062_EmptyStruct
}
