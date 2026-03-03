// RUN: %target-typecheck-verify-swift -strict-memory-safety -I %S/Inputs

import unsafe_decls

// expected-warning@+3{{struct 'StoreAllTheThings' has storage involving unsafe types}}
// expected-note@+2{{add '@unsafe' if this type is also unsafe to use}}
// expected-note@+1{{add '@safe' if this type encapsulates the unsafe storage in a safe interface}}
struct StoreAllTheThings {
  let np: NoPointers
  let npu: NoPointersUnion
  let npu2: NoPointersUnsafe // expected-note{{property 'npu2' involves unsafe type 'NoPointersUnsafe'}}

  let hp: HasPointers // expected-note{{property 'hp' involves unsafe type 'HasPointers'}}
  let hpu: HasPointersUnion // expected-note{{property 'hpu' involves unsafe type 'HasPointersUnion'}}
  let nps: HasPointersSafe

  let hrc: HasRefCounted

  let ln: ListNode // expected-note{{property 'ln' involves unsafe type 'ListNode'}}

  let sc: SomeColor
}

func readFromUnion(npu: NoPointersUnion) {
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = npu.x // expected-note{{reference to unsafe property 'x'}}
}
