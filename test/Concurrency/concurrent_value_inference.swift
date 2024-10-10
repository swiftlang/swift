// RUN: %target-swift-frontend -enable-library-evolution -strict-concurrency=complete %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend -enable-library-evolution -strict-concurrency=complete %s -emit-sil -o /dev/null -verify -enable-upcoming-feature RegionBasedIsolation

// REQUIRES: concurrency
// REQUIRES: swift_feature_RegionBasedIsolation

class C1 { } // expected-note {{class 'C1' does not conform to the 'Sendable' protocol}}
final class C2: Sendable { }

struct S1 {
  var x: Int
  var s: String
  var c: C2
}

enum E1 { // expected-note {{consider making enum 'E1' conform to the 'Sendable' protocol}}{{9-9=: Sendable}}
  case base
  indirect case nested(E1)
}

enum E2 {
  case s1(S1)
  case c2(C2)
}

struct GS1<T> { }

struct GS2<T> {  // expected-note {{consider making generic struct 'GS2' conform to the 'Sendable' protocol}}
  var storage: T
}

func acceptCV<T: Sendable>(_: T) { }

// Example that was triggering circular dependencies.
struct Signature { }
struct Data { }
struct BlockInfo { }

struct Bitcode { // expected-note {{consider making struct 'Bitcode' conform to the 'Sendable' protocol}}
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
public struct PublicStruct { // expected-note {{consider making struct 'PublicStruct' conform to the 'Sendable' protocol}}
  var i: Int
}

public enum PublicEnum { // expected-note {{consider making enum 'PublicEnum' conform to the 'Sendable' protocol}}
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

@available(SwiftStdlib 5.1, *)
@globalActor
actor MyGlobalActor {
  static let shared = MyGlobalActor()
}

@available(SwiftStdlib 5.1, *)
@MyGlobalActor
class C3 { }

@available(SwiftStdlib 5.1, *)
class C4: C3 { }

// Make Sendable unavailable, but be sure not to diagnose it.
struct S2 {
  var c1: C1
}

@available(*, unavailable)
extension S2: Sendable { }

@available(SwiftStdlib 5.1, *)
func testCV(
  c1: C1, c2: C2, c3: C3, c4: C4, s1: S1, e1: E1, e2: E2,
  gs1: GS1<Int>, gs2: GS2<Int>,
  bc: Bitcode, ps: PublicStruct, pe: PublicEnum,
  fps: FrozenPublicStruct, fpe: FrozenPublicEnum,
  hf: HasFunctions
) {
  acceptCV(c1) // expected-warning {{type 'C1' does not conform to the 'Sendable' protocol}}
  acceptCV(c2)
  acceptCV(c3)
  acceptCV(c4)
  acceptCV(s1)
  acceptCV(e1) // expected-warning {{type 'E1' does not conform to the 'Sendable'}}
  acceptCV(e2)
  acceptCV(gs1)
  acceptCV(gs2) // expected-warning {{type 'GS2<Int>' does not conform to the 'Sendable' protocol}}

  // Not available due to recursive conformance dependencies.
  acceptCV(bc) // expected-warning {{type 'Bitcode' does not conform to the 'Sendable' protocol}}

  // Not available due to "public".
  acceptCV(ps) // expected-warning {{type 'PublicStruct' does not conform to the 'Sendable' protocol}}
  acceptCV(pe) // expected-warning {{type 'PublicEnum' does not conform to the 'Sendable' protocol}}

  // Public is okay when also @frozen.
  acceptCV(fps)
  acceptCV(fpe)

  // Thin and C function types are Sendable.
  acceptCV(hf)
}
