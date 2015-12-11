// RUN: rm -rf %t && mkdir %t

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -c %S/../Inputs/resilient_struct.swift -o %t/resilient_struct.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -c %S/../Inputs/resilient_struct.swift -o %t/resilient_struct.o

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -c %S/../Inputs/resilient_enum.swift -I %t/ -o %t/resilient_enum.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -c %S/../Inputs/resilient_enum.swift -I %t/ -o %t/resilient_enum.o

// RUN: %target-build-swift %s -Xlinker %t/resilient_struct.o -Xlinker %t/resilient_enum.o -I %t -L %t -o %t/main
// RUN: %target-run %t/main

import StdlibUnittest
import resilient_enum
import resilient_struct

var ResilientEnumTestSuite = TestSuite("ResilientEnum")

ResilientEnumTestSuite.test("ResilientEmptyEnum") {
  let e = ResilientEmptyEnum.X
  let n: Int
  switch e {
  case .X: n = 0
  default: n = -1
  }
  expectEqual(n, 0)
}

ResilientEnumTestSuite.test("ResilientSingletonEnum") {
  let o: AnyObject = ArtClass()
  let e = ResilientSingletonEnum.X(o)
  let n: Int
  switch e {
  case .X(let oo):
    n = 0
    expectTrue(o === oo)
  default:
    n = -1
  }
  expectEqual(n, 0)
}

ResilientEnumTestSuite.test("ResilientSingletonGenericEnum") {
  let o = ArtClass()
  let e = ResilientSingletonGenericEnum.X(o)
  let n: Int
  switch e {
  case .X(let oo):
    n = 0
    expectEqual(o === oo, true)
  default:
    n = -1
  }
  expectEqual(n, 0)
}

ResilientEnumTestSuite.test("ResilientNoPayloadEnum") {
  let a: [ResilientNoPayloadEnum] = [.A, .B, .C]
  let b: [Int] = a.map {
    switch $0 {
    case .A:
      return 0
    case .B:
      return 1
    case .C:
      return 2
    default:
      return -1
    }
  }

  expectEqual(b, [0, 1, 2])
}

ResilientEnumTestSuite.test("ResilientSinglePayloadEnum") {
  let o = ArtClass()
  let a: [ResilientSinglePayloadEnum] = [.A, .B, .C, .X(o)]
  let b: [Int] = a.map {
    switch $0 {
    case .A:
      return 0
    case .B:
      return 1
    case .C:
      return 2
    case .X(let oo):
      expectTrue(o === oo)
      return 3
    default:
      return -1
    }
  }

  expectEqual(b, [0, 1, 2, 3])
}

ResilientEnumTestSuite.test("ResilientSinglePayloadGenericEnum") {
  let o = ArtClass()
  let a: [ResilientSinglePayloadGenericEnum<ArtClass>] = [.A, .B, .C, .X(o)]
  let b: [Int] = a.map {
    switch $0 {
    case .A:
      return 0
    case .B:
      return 1
    case .C:
      return 2
    case .X(let oo):
      expectTrue(o === oo)
      return 3
    default:
      return -1
    }
  }

  expectEqual(b, [0, 1, 2, 3])
}

ResilientEnumTestSuite.test("ResilientMultiPayloadEnum") {
  let a: [ResilientMultiPayloadEnum] =
      [.A, .B, .C, .X(1), .Y(2)]
  let b: [Int] = a.map {
    switch $0 {
    case .A:
      return 0
    case .B:
      return 1
    case .C:
      return 2
    case .X(let x):
      expectEqual(x, 1)
      return 3
    case .Y(let y):
      expectEqual(y, 2)
      return 4
    default:
      return -1
    }
  }

  expectEqual(b, [0, 1, 2, 3, 4])
}

ResilientEnumTestSuite.test("ResilientMultiPayloadEnumRoundTrip") {
  let a = [0, 1, 2, 3, 4]
  let b = a.map { makeResilientMultiPayloadEnum(1122, i: $0) }
  let c: [Int] = b.map {
    switch $0 {
    case .A:
      return 0
    case .B:
      return 1
    case .C:
      return 2
    case .X(let x):
      expectEqual(x, 1122)
      return 3
    case .Y(let y):
      expectEqual(y, 1122)
      return 4
    default:
      return -1
    }
  }

  expectEqual(c, a)
}

ResilientEnumTestSuite.test("ResilientMultiPayloadEnumSpareBits") {
  let o1 = ArtClass()
  let o2 = ArtClass()
  let a: [ResilientMultiPayloadEnumSpareBits] =
      [.A, .B, .C, .X(o1), .Y(o2)]
  let b: [Int] = a.map {
    switch $0 {
    case .A:
      return 0
    case .B:
      return 1
    case .C:
      return 2
    case .X(let oo1):
      expectTrue(oo1 === o1)
      return 3
    case .Y(let oo2):
      expectTrue(oo2 === o2)
      return 4
    default:
      return -1
    }
  }

  expectEqual(b, [0, 1, 2, 3, 4])
}

ResilientEnumTestSuite.test("ResilientMultiPayloadEnumSpareBitsRoundTrip") {
  let o = ArtClass()
  let a = [0, 1, 2, 3, 4]
  let b = a.map { makeResilientMultiPayloadEnumSpareBits(o, i: $0) }
  let c: [Int] = b.map {
    switch $0 {
    case .A:
      return 0
    case .B:
      return 1
    case .C:
      return 2
    case .X(let oo):
      expectTrue(oo === o)
      return 3
    case .Y(let oo):
      expectTrue(oo === o)
      return 4
    default:
      return -1
    }
  }

  expectEqual(c, a)
}

ResilientEnumTestSuite.test("ResilientMultiPayloadEnumSpareBitsAndExtraBits") {
  let o = ArtClass()
  let s: SevenSpareBits = (false, 1, 2, 3, 4, 5, 6, 7)
  let a: [ResilientMultiPayloadEnumSpareBitsAndExtraBits]
      = [.P1(s), .P2(o), .P3(o), .P4(o), .P5(o), .P6(o), .P7(o), .P8(o)]
  let b: [Int] = a.map {
    switch $0 {
    case .P1(let ss):
      // FIXME: derive Equatable conformances for arbitrary tuples :-) 
      expectEqual(ss.0, s.0)
      expectEqual(ss.1, s.1)
      expectEqual(ss.2, s.2)
      expectEqual(ss.3, s.3)
      expectEqual(ss.4, s.4)
      expectEqual(ss.5, s.5)
      expectEqual(ss.6, s.6)
      expectEqual(ss.7, s.7)
      return 0
    case .P2(let oo):
      expectTrue(oo === o)
      return 1
    case .P3(let oo):
      expectTrue(oo === o)
      return 2
    case .P4(let oo):
      expectTrue(oo === o)
      return 3
    case .P5(let oo):
      expectTrue(oo === o)
      return 4
    case .P6(let oo):
      expectTrue(oo === o)
      return 5
    case .P7(let oo):
      expectTrue(oo === o)
      return 6
    case .P8(let oo):
      expectTrue(oo === o)
      return 7
    default:
      return -1
    }
  }

  expectEqual(b, [0, 1, 2, 3, 4, 5, 6, 7])
}

ResilientEnumTestSuite.test("ResilientMultiPayloadGenericEnum") {
  let o1 = ArtClass()
  let o2 = ArtClass()
  let a: [ResilientMultiPayloadGenericEnum<ArtClass>] =
      [.A, .B, .C, .X(o1), .Y(o2)]
  let b: [Int] = a.map {
    switch $0 {
    case .A:
      return 0
    case .B:
      return 1
    case .C:
      return 2
    case .X(let oo1):
      expectTrue(oo1 === o1)
      return 3
    case .Y(let oo2):
      expectTrue(oo2 === o2)
      return 4
    default:
      return -1
    }
  }

  expectEqual(b, [0, 1, 2, 3, 4])
}

runAllTests()
