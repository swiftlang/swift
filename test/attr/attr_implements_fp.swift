// RUN: %empty-directory(%t)
// RUN: echo 'main()' >%t/main.swift
// RUN: %target-swiftc_driver -o %t/a.out %s %t/main.swift
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
// REQUIRES: executable_test

// This is a more-thorough and explicit test for rdar://43804798 that uses @_implements to
// achieve "Comparable Floating Point values are FP-like when known to be FP, Comparable-like
// when only known to be comparable".

// Could calls to the different comparison operators.
public var comparedAsComparablesCount : Int = 0
public var comparedAsFauxtsCount : Int = 0

public protocol FauxtingPoint : Comparable {
  static var nan: Self { get }
  static var one: Self { get }
  static var two: Self { get }
}

public protocol BinaryFauxtingPoint: FauxtingPoint {
  var bitPattern: UInt8 { get }
}

public extension BinaryFauxtingPoint {
  // This version of < will be called in a context that only knows it has a Comparable.
  @_implements(Comparable, <(_:_:))
  static func _ComparableLessThan(_ lhs: Fauxt, _ rhs: Fauxt) -> Bool {
    print("compared as Comparables")
    comparedAsComparablesCount += 1
    return lhs.bitPattern < rhs.bitPattern
  }
}

public enum State {
  case Nan
  case One
  case Two
}

public struct Fauxt {
  let state: State
  init(_ s: State) {
    state = s
  }
  public static var nan: Fauxt {
    return Fauxt(State.Nan)
  }
  public static var one: Fauxt {
    return Fauxt(State.One)
  }
  public static var two: Fauxt {
    return Fauxt(State.Two)
  }
}

extension Fauxt: BinaryFauxtingPoint {
  // Requirement from BinaryFauxtingPoint
  public var bitPattern: UInt8 {
    switch state {
    case .One:
      return 1
    case .Two:
      return 2
    case .Nan:
      return 0xff
    }
  }
}

public extension Fauxt {
  // This version of < will be called in a context that knows it has a Fauxt.
  // It is inside an extension of Fauxt rather than the declaration of Fauxt
  // itself in order to avoid a warning about near-matches with the defaulted
  // requirement from Comparable.< up above.
  static func <(_ lhs: Fauxt, _ rhs: Fauxt) -> Bool {
    print("compared as Fauxts")
    comparedAsFauxtsCount += 1
    if lhs.state == .Nan || rhs.state == .Nan {
      return false
    } else {
      return lhs.bitPattern < rhs.bitPattern
    }
  }
}

public func compare_Comparables<T:Comparable>(_ x: T, _ y: T) -> Bool {
  return x < y
}

public func compare_FauxtingPoint<T:FauxtingPoint>(_ x: T, _ y: T) -> Bool {
  return x < y
}

public func compare_Fauxts(_ x: Fauxt, _ y: Fauxt) -> Bool {
  return x < y
}

public func main() {
  assert(compare_Comparables(Fauxt.one, Fauxt.two))
  assert(comparedAsComparablesCount == 1)
  // CHECK: compared as Comparables
  assert(compare_Comparables(Fauxt.one, Fauxt.nan))
  assert(comparedAsComparablesCount == 2)
  // CHECK: compared as Comparables
  assert(!compare_Comparables(Fauxt.nan, Fauxt.one))
  assert(comparedAsComparablesCount == 3)
  // CHECK: compared as Comparables

  assert(compare_FauxtingPoint(Fauxt.one, Fauxt.two))
  assert(comparedAsComparablesCount == 4)
  // CHECK: compared as Comparables
  assert(compare_FauxtingPoint(Fauxt.one, Fauxt.nan))
  assert(comparedAsComparablesCount == 5)
  // CHECK: compared as Comparables
  assert(!compare_FauxtingPoint(Fauxt.nan, Fauxt.one))
  assert(comparedAsComparablesCount == 6)
  // CHECK: compared as Comparables

  assert(compare_Fauxts(Fauxt.one, Fauxt.two))
  assert(comparedAsFauxtsCount == 1)
  // CHECK: compared as Fauxts
  assert(!compare_Fauxts(Fauxt.one, Fauxt.nan))
  assert(comparedAsFauxtsCount == 2)
  // CHECK: compared as Fauxts
  assert(!compare_Fauxts(Fauxt.nan, Fauxt.one))
  assert(comparedAsFauxtsCount == 3)
  // CHECK: compared as Fauxts
}
