// RUN: %target-run-simple-swift(-Xfrontend -enable-type-layout -Xfrontend -force-struct-type-layouts -Xfrontend -enable-autolinking-runtime-compatibility-bytecode-layouts -lc++) --stdlib-unittest-in-process

// REQUIRES: executable_test

import StdlibUnittest

var Tests = TestSuite("RuntimeValueWitness")

Tests.test("MultiReferenceStruct") {
  @_GenerateLayoutBytecode
  struct LifetimeStruct {
    init(a: LifetimeTracked, b: LifetimeTracked, c: LifetimeTracked) {
      self.a = a
      self.b = b
      self.c = c
    }
    let a: LifetimeTracked
    let b: LifetimeTracked
    let c: LifetimeTracked
  }
  let _ = LifetimeStruct(a: LifetimeTracked(0), b: LifetimeTracked(0), c: LifetimeTracked(0))
}

Tests.test("AlignedStruct") {
  @_GenerateLayoutBytecode
  struct AlignedStruct {
    init(a: UInt8, b: UInt16, c: LifetimeTracked) {
      self.a = a
      self.b = b
      self.c = c
    }
    let a: UInt8
    let b: UInt16
    let c: LifetimeTracked
  }
  let _ = AlignedStruct(a: 0xAA, b: 0xBBBB, c: LifetimeTracked(0))
}

Tests.test("NestedStruct") {
  @_GenerateLayoutBytecode
  struct NestedStruct {
    init(a: UInt8, b: LifetimeTracked) {
      self.a = a
      self.b = b
    }
    let a: UInt8
    let b: LifetimeTracked
  }
  @_GenerateLayoutBytecode
  struct OuterStruct {
    init(a: UInt8, b: NestedStruct) {
      self.a = a
      self.b = b
    }
    let a: UInt8
    let b: NestedStruct
  }
  // We should expect to see a layout of AA00000000000000 BB00000000000000 POINTER
  // As the nested struct, and thus b, will be pointer aligned
  let _ = OuterStruct(a: 0xAA, b: NestedStruct(a: 0xBB, b: LifetimeTracked(0)))
}

Tests.test("FlattenedStruct") {
  @_GenerateLayoutBytecode
  struct FlattenedStruct {
    init(a: UInt8, b: UInt8, c: LifetimeTracked) {
      self.a = a
      self.b = b
      self.c = c
    }
    let a: UInt8
    let b: UInt8
    let c: LifetimeTracked
  }
  // We should expect to see a layout of AABB000000000000 POINTER
  // As the layout should pack a and b.
  let _ = FlattenedStruct(a: 0xAA, b: 0xBB, c: LifetimeTracked(0))
}

Tests.test("NoPayloadEnumStruct") {
  @_GenerateLayoutBytecode
  enum NoPayload {
    case Only
    case NonPayload
    case Cases
  }
  @_GenerateLayoutBytecode
  struct NoPayloadEnumStruct {
    init(a: NoPayload, c: LifetimeTracked) {
      self.a = a
      self.c = c
    }
    let a: NoPayload
    let c: LifetimeTracked
  }
  let _ = NoPayloadEnumStruct(a: .Cases, c: LifetimeTracked(0))
}

Tests.test("SinglePayloadEnumStruct") {
  @_GenerateLayoutBytecode
  enum SinglePayload {
    case Payload(c: LifetimeTracked)
    case NoPayload
  }
  @_GenerateLayoutBytecode
  struct SinglePayloadEnumStruct {
    init(a: SinglePayload, c: LifetimeTracked) {
      self.a = a
      self.c = c
    }
    let a: SinglePayload
    let c: LifetimeTracked
  }
  let _ = SinglePayloadEnumStruct(a: .Payload(c: LifetimeTracked(0)), c: LifetimeTracked(0))
  let _ = SinglePayloadEnumStruct(a: .NoPayload, c: LifetimeTracked(0))
}

Tests.test("Nested Enum") {
  @_GenerateLayoutBytecode
  enum SinglePayload {
    case Payload(c: LifetimeTracked)
    case NoPayload
  }
  @_GenerateLayoutBytecode
  enum SingleEnumPayload {
    case EnumPayload(e: SinglePayload)
    case NoEnumPayload
  }
  @_GenerateLayoutBytecode
  struct SingleEnumPayloadEnumStruct {
    init(a: SingleEnumPayload, c: LifetimeTracked) {
      self.a = a
      self.c = c
    }
    let a: SingleEnumPayload
    let c: LifetimeTracked
  }
  let _ = SingleEnumPayloadEnumStruct(a: .EnumPayload(e: .Payload(c: LifetimeTracked(0))), c: LifetimeTracked(0))
  let _ = SingleEnumPayloadEnumStruct(a: .EnumPayload(e: .NoPayload), c: LifetimeTracked(0))
  let _ = SingleEnumPayloadEnumStruct(a: .NoEnumPayload, c: LifetimeTracked(0))
}

Tests.test("MultiEnum") {
  @_GenerateLayoutBytecode
  enum MultiPayload {
    case Payload1(c: LifetimeTracked)
    case Payload2(c: LifetimeTracked, d: LifetimeTracked)
    case Payload3(c: LifetimeTracked, d: LifetimeTracked)
    case Payload4(c: LifetimeTracked, d: LifetimeTracked)
    case Payload5(c: LifetimeTracked, d: LifetimeTracked)
    case Payload6(c: LifetimeTracked, d: LifetimeTracked)
    case Payload7(c: LifetimeTracked, d: LifetimeTracked)
    case Payload8(c: LifetimeTracked, d: LifetimeTracked)
    case Payload9(c: LifetimeTracked, d: LifetimeTracked)
    case Payload10(c: LifetimeTracked, d: LifetimeTracked, e: LifetimeTracked)
    case Payload11(c: LifetimeTracked, d: LifetimeTracked, e: LifetimeTracked, f: UInt64)
    case NoPayload
    case NoPayload2
    case NoPayload3
    case NoPayload4
  }

  @_GenerateLayoutBytecode
  struct MultiPayloadStruct {
    init(a: MultiPayload, c: LifetimeTracked) {
      self.a = a
      self.c = c
    }
    let a: MultiPayload
    let c: LifetimeTracked
  }
  let _ = MultiPayloadStruct(a: .Payload1(c: LifetimeTracked(0)), c: LifetimeTracked(0))
  let _ = MultiPayloadStruct(a: .Payload2(c: LifetimeTracked(0), d: LifetimeTracked(0)), c: LifetimeTracked(0))
  let _ = MultiPayloadStruct(a: .Payload3(c: LifetimeTracked(0), d: LifetimeTracked(0)), c: LifetimeTracked(0))
  let _ = MultiPayloadStruct(a: .Payload4(c: LifetimeTracked(0), d: LifetimeTracked(0)), c: LifetimeTracked(0))
  let _ = MultiPayloadStruct(a: .Payload5(c: LifetimeTracked(0), d: LifetimeTracked(0)), c: LifetimeTracked(0))
  let _ = MultiPayloadStruct(a: .Payload6(c: LifetimeTracked(0), d: LifetimeTracked(0)), c: LifetimeTracked(0))
  let _ = MultiPayloadStruct(a: .Payload7(c: LifetimeTracked(0), d: LifetimeTracked(0)), c: LifetimeTracked(0))
  let _ = MultiPayloadStruct(a: .Payload8(c: LifetimeTracked(0), d: LifetimeTracked(0)), c: LifetimeTracked(0))
  let _ = MultiPayloadStruct(a: .Payload9(c: LifetimeTracked(0), d: LifetimeTracked(0)), c: LifetimeTracked(0))
  let _ = MultiPayloadStruct(a: .Payload10(c: LifetimeTracked(0), d: LifetimeTracked(0), e: LifetimeTracked(0)), c: LifetimeTracked(0))
  let _ = MultiPayloadStruct(a: .Payload11(c: LifetimeTracked(0), d: LifetimeTracked(0), e: LifetimeTracked(0), f: 0xAAAAAAAAAAAAAAAA), c: LifetimeTracked(0))
  let _ = MultiPayloadStruct(a: .NoPayload, c: LifetimeTracked(0))
  let _ = MultiPayloadStruct(a: .NoPayload2, c: LifetimeTracked(0))
  let _ = MultiPayloadStruct(a: .NoPayload3, c: LifetimeTracked(0))
  let _ = MultiPayloadStruct(a: .NoPayload4, c: LifetimeTracked(0))
}

runAllTests()
