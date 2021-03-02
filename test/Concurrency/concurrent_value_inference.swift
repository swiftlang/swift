// RUN: %target-typecheck-verify-swift

class C1 { }
final class C2: ConcurrentValue { }

struct S1 {
  var x: Int
  var s: String
  var c: C2
}

enum E1 {
  case base
  indirect case nested(E1)
}

enum E2 {
  case s1(S1)
  case c2(C2)
}

struct GS1<T> { }

struct GS2<T> {
  var storage: T
}

func acceptCV<T: ConcurrentValue>(_: T) { }
// expected-note@-1 4{{where 'T' =}}

// Example that was triggering circular dependencies.
struct Signature { }
struct Data { }
struct BlockInfo { }

struct Bitcode {
  let signature: Signature
  let elements: [BitcodeElement]
  let blockInfo: [UInt64: BlockInfo]
}

enum BitcodeElement {
  struct Block {
    var id: UInt64
    var elements: [BitcodeElement]
  }

  struct Record {
    enum Payload {
      case none
      case array([UInt64])
      case char6String(String)
      case blob(Data)
    }

    var id: UInt64
    var fields: [UInt64]
    var payload: Payload
  }

  case block(Block)
  case record(Record)
}


func testCV(
  c1: C1, c2: C2, s1: S1, e1: E1, e2: E2, gs1: GS1<Int>, gs2: GS2<Int>,
  bc: Bitcode
) {
  acceptCV(c1) // expected-error{{'C1' conform to 'ConcurrentValue'}}
  acceptCV(c2)
  acceptCV(s1)
  acceptCV(e1) // expected-error{{'E1' conform to 'ConcurrentValue'}}
  acceptCV(e2)
  acceptCV(gs1)
  acceptCV(gs2) // expected-error{{'GS2<Int>' conform to 'ConcurrentValue'}}

  // Note available due to recursive conformance dependencies.
  acceptCV(bc) // expected-error{{global function 'acceptCV' requires that 'Bitcode' conform to 'ConcurrentValue'}}
}
