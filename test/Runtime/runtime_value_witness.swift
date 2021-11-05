// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(resilient_struct)) %S/../Inputs/resilient_struct.swift -enable-library-evolution -emit-module -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct
// RUN: %target-build-swift-dylib(%t/%target-library-name(resilient_enum)) %S/../Inputs/resilient_enum.swift -enable-library-evolution -emit-module -emit-module-path=%t/resilient_enum.swiftmodule -module-name=resilient_enum -I %t -L %t -lresilient_struct
// RUN: %target-build-swift -g -Xfrontend -enable-type-layout -Xfrontend -force-struct-type-layouts -Xfrontend -enable-autolinking-runtime-compatibility-bytecode-layouts -I %t -L %t -lresilient_struct -lresilient_enum %target-cxx-lib %s %target-rpath(%t) -o %t/a.out
// RUN: %target-run %t/a.out --stdlib-unittest-in-process

// REQUIRES: executable_test

import StdlibUnittest
import resilient_struct
import resilient_enum

var Tests = TestSuite("RuntimeValueWitness")


func assignCopy<T>(_ a: T, _ b: inout T) {
  b = a
}

func assignTake<T>(_ a: T, _ b: inout T) {
  let ret1: T = a
  b = ret1
}

func initCopy<T>(_ a: T) -> T {
  let ret: T = a
  return ret
}

func initTake<T>(_ a: T) -> T {
  let ret1: T = a
  let ret2: T = ret1
  return ret2
}

func checkCopies<T: Equatable>(_ a: T, _ b: inout T) {
  let savedB: T = b
  assignCopy(a, &b)
  expectEqual(a, b)

  b = savedB
  assignTake(a, &b)
  expectEqual(a, b)

  expectEqual(a, initCopy(a))
  expectEqual(a, initTake(a))
}

Tests.test("MultiReferenceStruct") {
  @_GenerateLayoutBytecode
  struct LifetimeStruct : Equatable {
    init(a: LifetimeTracked, b: LifetimeTracked, c: LifetimeTracked) {
      self.a = a
      self.b = b
      self.c = c
    }
    let a: LifetimeTracked
    let b: LifetimeTracked
    let c: LifetimeTracked
  }
  let a = LifetimeStruct(a: LifetimeTracked(0), b: LifetimeTracked(0), c: LifetimeTracked(0))
  var b = LifetimeStruct(a: LifetimeTracked(1), b: LifetimeTracked(1), c: LifetimeTracked(1))
  checkCopies(a, &b)
}

Tests.test("AlignedStruct") {
  @_GenerateLayoutBytecode
  struct AlignedStruct: Equatable {
    init(a: UInt8, b: UInt16, c: LifetimeTracked) {
      self.a = a
      self.b = b
      self.c = c
    }
    let a: UInt8
    let b: UInt16
    let c: LifetimeTracked
  }
  let a = AlignedStruct(a: 0xAA, b: 0xBBBB, c: LifetimeTracked(0))
  var b = AlignedStruct(a: 0xDD, b: 0xEEEE, c: LifetimeTracked(0))
  checkCopies(a, &b)
}

Tests.test("NestedStruct") {
  @_GenerateLayoutBytecode
  struct NestedStruct: Equatable {
    init(a: UInt8, b: LifetimeTracked) {
      self.a = a
      self.b = b
    }
    let a: UInt8
    let b: LifetimeTracked
  }
  @_GenerateLayoutBytecode
  struct OuterStruct: Equatable {
    init(a: UInt8, b: NestedStruct) {
      self.a = a
      self.b = b
    }
    let a: UInt8
    let b: NestedStruct
  }
  // We should expect to see a layout of AA00000000000000 BB00000000000000 POINTER
  // As the nested struct, and thus b, will be pointer aligned
  let a = OuterStruct(a: 0xAA, b: NestedStruct(a: 0xBB, b: LifetimeTracked(0)))
  var b = OuterStruct(a: 0xCC, b: NestedStruct(a: 0xDD, b: LifetimeTracked(1)))
  checkCopies(a, &b)
}

Tests.test("FlattenedStruct") {
  @_GenerateLayoutBytecode
  struct FlattenedStruct: Equatable {
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
  let a = FlattenedStruct(a: 0xAA, b: 0xBB, c: LifetimeTracked(0))
  var b = FlattenedStruct(a: 0xCC, b: 0xDD, c: LifetimeTracked(1))
  checkCopies(a, &b)
}

Tests.test("NoPayloadEnumStruct") {
  @_GenerateLayoutBytecode
  enum NoPayload : Equatable {
    case Only
    case NonPayload
    case Cases
  }
  @_GenerateLayoutBytecode
  struct NoPayloadEnumStruct : Equatable {
    init(a: NoPayload, c: LifetimeTracked) {
      self.a = a
      self.c = c
    }
    let a: NoPayload
    let c: LifetimeTracked
  }
  let a = NoPayloadEnumStruct(a: .Cases, c: LifetimeTracked(0))
  var b = NoPayloadEnumStruct(a: .Only, c: LifetimeTracked(1))
  checkCopies(a, &b)
}

Tests.test("SinglePayloadEnumStruct") {
  @_GenerateLayoutBytecode
  enum SinglePayload : Equatable {
    case Payload(c: LifetimeTracked)
    case NoPayload
  }
  @_GenerateLayoutBytecode
  struct SinglePayloadEnumStruct : Equatable {
    init(a: SinglePayload, c: LifetimeTracked) {
      self.a = a
      self.c = c
    }
    let a: SinglePayload
    let c: LifetimeTracked
  }
  let a = SinglePayloadEnumStruct(a: .Payload(c: LifetimeTracked(0)), c: LifetimeTracked(0))
  var b1 = SinglePayloadEnumStruct(a: .NoPayload, c: LifetimeTracked(1))
  var b2 = SinglePayloadEnumStruct(a: .Payload(c: LifetimeTracked(1)), c: LifetimeTracked(1))
  checkCopies(a, &b1)
  checkCopies(a, &b2)

  let c = SinglePayloadEnumStruct(a: .NoPayload, c: LifetimeTracked(0))
  var d1 = SinglePayloadEnumStruct(a: .NoPayload, c: LifetimeTracked(1))
  var d2 = SinglePayloadEnumStruct(a: .Payload(c: LifetimeTracked(0)), c: LifetimeTracked(0))
  checkCopies(c, &d1)
  checkCopies(c, &d2)
}

Tests.test("Nested Enum") {
  @_GenerateLayoutBytecode
  enum SinglePayload: Equatable {
    case Payload(c: LifetimeTracked)
    case NoPayload
  }
  @_GenerateLayoutBytecode
  enum SingleEnumPayload: Equatable {
    case EnumPayload(e: SinglePayload)
    case NoEnumPayload
  }
  @_GenerateLayoutBytecode
  struct SingleEnumPayloadEnumStruct : Equatable {
    init(a: SingleEnumPayload, c: LifetimeTracked) {
      self.a = a
      self.c = c
    }
    let a: SingleEnumPayload
    let c: LifetimeTracked
  }
  let a = SingleEnumPayloadEnumStruct(a: .EnumPayload(e: .Payload(c: LifetimeTracked(0))), c: LifetimeTracked(0))
  var a1 = SingleEnumPayloadEnumStruct(a: .EnumPayload(e: .Payload(c: LifetimeTracked(1))), c: LifetimeTracked(1))
  checkCopies(a, &a1)
  var a2 = SingleEnumPayloadEnumStruct(a: .EnumPayload(e: .NoPayload), c: LifetimeTracked(1))
  checkCopies(a, &a2)
  var a3 = SingleEnumPayloadEnumStruct(a: .NoEnumPayload, c: LifetimeTracked(1))
  checkCopies(a, &a3)

  let b = SingleEnumPayloadEnumStruct(a: .EnumPayload(e: .NoPayload), c: LifetimeTracked(0))
  var b1 = SingleEnumPayloadEnumStruct(a: .EnumPayload(e: .Payload(c: LifetimeTracked(1))), c: LifetimeTracked(1))
  checkCopies(b, &b1)
  var b2 = SingleEnumPayloadEnumStruct(a: .EnumPayload(e: .NoPayload), c: LifetimeTracked(1))
  checkCopies(b, &b2)
  var b3 = SingleEnumPayloadEnumStruct(a: .NoEnumPayload, c: LifetimeTracked(1))
  checkCopies(b, &b3)

  let c = SingleEnumPayloadEnumStruct(a: .NoEnumPayload, c: LifetimeTracked(0))
  var c1 = SingleEnumPayloadEnumStruct(a: .EnumPayload(e: .Payload(c: LifetimeTracked(1))), c: LifetimeTracked(1))
  checkCopies(c, &c1)
  var c2 = SingleEnumPayloadEnumStruct(a: .EnumPayload(e: .NoPayload), c: LifetimeTracked(1))
  checkCopies(c, &c2)
  var c3 = SingleEnumPayloadEnumStruct(a: .NoEnumPayload, c: LifetimeTracked(1))
  checkCopies(c, &c3)
}

Tests.test("Archetypes") {
  @_GenerateLayoutBytecode
  struct ArchetypeStruct<T: Equatable> : Equatable {
    init(a: T, b: LifetimeTracked) {
      self.a = a
      self.b = b
    }
    let a: T
    let b: LifetimeTracked
  }

  let a = ArchetypeStruct<UInt64>(a: 0xAAAA, b: LifetimeTracked(0))
  var b = ArchetypeStruct<UInt64>(a: 0xBBBB, b: LifetimeTracked(1))
  checkCopies(a, &b)

  let c = ArchetypeStruct<UInt32>(a: 0xBBBB, b: LifetimeTracked(0))
  var d = ArchetypeStruct<UInt32>(a: 0xCCCC, b: LifetimeTracked(1))
  checkCopies(c, &d)

  let e = ArchetypeStruct<UInt16>(a: 0xCCCC, b: LifetimeTracked(0))
  var f = ArchetypeStruct<UInt16>(a: 0xDDDD, b: LifetimeTracked(1))
  checkCopies(e, &f)

  let g = ArchetypeStruct<UInt8>(a: 0xDD, b: LifetimeTracked(0))
  var h = ArchetypeStruct<UInt8>(a: 0xEE, b: LifetimeTracked(1))
  checkCopies(g, &h)

  let i = ArchetypeStruct<LifetimeTracked>(a: LifetimeTracked(0), b: LifetimeTracked(0))
  var j = ArchetypeStruct<LifetimeTracked>(a: LifetimeTracked(1), b: LifetimeTracked(2))
  checkCopies(i, &j)
}

Tests.test("Multi Archetypes") {
  @_GenerateLayoutBytecode
  struct ArchetypeStruct<S: Equatable, T: Equatable> : Equatable {
    init(a: S, b: LifetimeTracked, c: T) {
      self.a = a
      self.b = b
      self.c = c
    }
    let a: S
    let b: LifetimeTracked
    let c: T
  }

  let a = ArchetypeStruct<LifetimeTracked, LifetimeTracked>(a: LifetimeTracked(0), b: LifetimeTracked(0), c: LifetimeTracked(0))
  var b = ArchetypeStruct<LifetimeTracked, LifetimeTracked>(a: LifetimeTracked(1), b: LifetimeTracked(1), c: LifetimeTracked(1))
  checkCopies(a, &b)

  let c = ArchetypeStruct<UInt64, UInt64>(a: 0xAAAA, b: LifetimeTracked(0), c: 0)
  var d = ArchetypeStruct<UInt64, UInt64>(a: 0xBBBB, b: LifetimeTracked(1), c: 1)
  checkCopies(c, &d)

  let e = ArchetypeStruct<UInt64, LifetimeTracked>(a: 0xAAAA, b: LifetimeTracked(0), c: LifetimeTracked(0))
  var f = ArchetypeStruct<UInt64, LifetimeTracked>(a: 0xBBBB, b: LifetimeTracked(1), c: LifetimeTracked(1))
  checkCopies(e, &f)
}

@_GenerateLayoutBytecode
struct ResilientStruct<T: Equatable> {
  init(a: T, b: Point, c: ResilientSinglePayloadGenericEnum<LifetimeTracked>, d: LifetimeTracked) {
    self.a = a
    self.b = b
    self.c = c
    self.d = d
  }
  let a: T
  let b: Point
  let c: ResilientSinglePayloadGenericEnum<LifetimeTracked>
  let d: LifetimeTracked
}

extension ResilientStruct : Equatable {
    static func == (lhs: ResilientStruct<T>, rhs: ResilientStruct<T>) -> Bool {
      return true
    }
}

Tests.test("Resilient") {
  let a = ResilientStruct<UInt16>(a: 0xFF, b: Point(x: 0,y: 0), c: .X(LifetimeTracked(0)), d: LifetimeTracked(0))
  var b1 = ResilientStruct<UInt16>(a: 0xEE, b: Point(x: 1,y: 2), c: .A, d: LifetimeTracked(1))
  var b2 = ResilientStruct<UInt16>(a: 0xEE, b: Point(x: 1,y: 2), c: .X(LifetimeTracked(1)), d: LifetimeTracked(1))
  checkCopies(a, &b1)
  checkCopies(a, &b2)

  let c = ResilientStruct<UInt16>(a: 0xFF, b: Point(x: 0,y: 0), c: .A, d: LifetimeTracked(0))
  var d1 = ResilientStruct<UInt16>(a: 0xEE, b: Point(x: 1,y: 2), c: .X(LifetimeTracked(1)), d: LifetimeTracked(0))
  var d2 = ResilientStruct<UInt16>(a: 0xEE, b: Point(x: 1,y: 2), c: .B, d: LifetimeTracked(0))
  checkCopies(c, &d1)
  checkCopies(c, &d2)

  let e = ResilientStruct<UInt16>(a: 0xFF, b: Point(x: 0,y: 0), c: .B, d: LifetimeTracked(0))
  var f1 = ResilientStruct<UInt16>(a: 0xEE, b: Point(x: 1,y: 2), c: .X(LifetimeTracked(1)), d: LifetimeTracked(0))
  var f2 = ResilientStruct<UInt16>(a: 0xEE, b: Point(x: 1,y: 2), c: .C, d: LifetimeTracked(0))
  checkCopies(e, &f1)
  checkCopies(e, &f2)

  let g = ResilientStruct<UInt16>(a: 0xFF, b: Point(x: 0,y: 0), c: .C, d: LifetimeTracked(0))
  var h1 = ResilientStruct<UInt16>(a: 0xEE, b: Point(x: 1,y: 2), c: .X(LifetimeTracked(1)), d: LifetimeTracked(0))
  var h2 = ResilientStruct<UInt16>(a: 0xEE, b: Point(x: 1,y: 2), c: .A, d: LifetimeTracked(0))
  checkCopies(g, &h1)
  checkCopies(g, &h2)
}

Tests.test("Archetype Enums") {
  @_GenerateLayoutBytecode
  enum ArchetypeEnum<T: Equatable> : Equatable {
    case Some(a: T)
    case None
  }

  let a = ArchetypeEnum<LifetimeTracked>.Some(a: LifetimeTracked(0))
  var b = ArchetypeEnum<LifetimeTracked>.Some(a: LifetimeTracked(1))
  checkCopies(a, &b)

  let b1 = ArchetypeEnum<LifetimeTracked>.Some(a: LifetimeTracked(1))
  var b2 = ArchetypeEnum<LifetimeTracked>.None
  checkCopies(b1, &b2)

  let c1 = ArchetypeEnum<LifetimeTracked>.None
  var c2 = ArchetypeEnum<LifetimeTracked>.Some(a: LifetimeTracked(2))
  checkCopies(c1, &c2)

  let d1 = ArchetypeEnum<LifetimeTracked>.None
  var d2 = ArchetypeEnum<LifetimeTracked>.None
  checkCopies(d1, &d2)
}

Tests.test("Bridging") {
  @_GenerateLayoutBytecode
  struct ArrayStruct : Equatable {
    init(a: Array<LifetimeTracked>, b: LifetimeTracked) {
      self.a = a
      self.b = b
    }
    let a: Array<LifetimeTracked>
    let b: LifetimeTracked
  }
  let a = ArrayStruct(a: [LifetimeTracked(1), LifetimeTracked(2), LifetimeTracked(3)], b: LifetimeTracked(0))
  var b = ArrayStruct(a: [LifetimeTracked(4), LifetimeTracked(5), LifetimeTracked(6)], b: LifetimeTracked(0))
  checkCopies(a, &b)
}

Tests.test("Bridging Optional") {
  @_GenerateLayoutBytecode
  struct ArrayStruct : Equatable {
    init(a: Array<LifetimeTracked>?, b: LifetimeTracked) {
      self.a = a
      self.b = b
    }
    let a: Array<LifetimeTracked>?
    let b: LifetimeTracked
  }
  let a = ArrayStruct(a: [LifetimeTracked(1), LifetimeTracked(2), LifetimeTracked(3)], b: LifetimeTracked(0))
  var b = ArrayStruct(a: [LifetimeTracked(4), LifetimeTracked(5), LifetimeTracked(6)], b: LifetimeTracked(0))
  checkCopies(a, &b)
}

runAllTests()
