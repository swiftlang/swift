// RUN: %target-typecheck-verify-swift -enable-library-evolution
// REQUIRES: concurrency

class C1 { }
final class C2: Sendable { }

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

func acceptCV<T: Sendable>(_: T) { }
// expected-note@-1 6{{where 'T' =}}

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

// Public structs and enums do not get implicit Sendable unless they
// are frozen.
public struct PublicStruct {
  var i: Int
}

public enum PublicEnum {
  case some
}

@frozen public struct FrozenPublicStruct {
  var i: Int
}

@frozen public enum FrozenPublicEnum {
  case some
}

struct HasFunctions {
  var tfp: @convention(thin) () -> Void
  var cfp: @convention(c) () -> Void
}

@globalActor
actor MyGlobalActor {
  static let shared = MyGlobalActor()
}

@MyGlobalActor
class C3 { }

class C4: C3 { }

func testCV(
  c1: C1, c2: C2, c3: C3, c4: C4, s1: S1, e1: E1, e2: E2,
  gs1: GS1<Int>, gs2: GS2<Int>,
  bc: Bitcode, ps: PublicStruct, pe: PublicEnum,
  fps: FrozenPublicStruct, fpe: FrozenPublicEnum,
  hf: HasFunctions
) {
  acceptCV(c1) // expected-error{{'C1' conform to 'Sendable'}}
  acceptCV(c2)
  acceptCV(c3)
  acceptCV(c4)
  acceptCV(s1)
  acceptCV(e1) // expected-error{{'E1' conform to 'Sendable'}}
  acceptCV(e2)
  acceptCV(gs1)
  acceptCV(gs2) // expected-error{{'GS2<Int>' conform to 'Sendable'}}

  // Not available due to recursive conformance dependencies.
  acceptCV(bc) // expected-error{{global function 'acceptCV' requires that 'Bitcode' conform to 'Sendable'}}

  // Not available due to "public".
  acceptCV(ps) // expected-error{{global function 'acceptCV' requires that 'PublicStruct' conform to 'Sendable'}}
  acceptCV(pe) // expected-error{{global function 'acceptCV' requires that 'PublicEnum' conform to 'Sendable'}}

  // Public is okay when also @frozen.
  acceptCV(fps)
  acceptCV(fpe)

  // Thin and C function types are Sendable.
  acceptCV(hf)
}
