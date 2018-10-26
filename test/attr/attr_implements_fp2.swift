// RUN: %empty-directory(%t)
// RUN: echo 'main()' >%t/main.swift
// RUN: %target-swiftc_driver -o %t/a.out %s %t/main.swift
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
// REQUIRES: executable_test

// This is a yet-more-thorough test for rdar://43804798 (and its successor rdar://45183426) that
// uses @_implements to achieve "Comparable Floating Point values are FP-like when known to be FP,
// Comparable-like when only known to be comparable".
//
// In this variant, the switch in behaviour happens at an intermediate protocol (FauxtingPoint)
// rather than at concrete types (Fauxt).
//
// Unfortunately to make this work, we have to go even further into the weeds and make use of
// @_nonoverride to provide a protocol-level entry to rank against Comparable.<, because the type
// system happens to prefer protocol-level entries to extension-level entries when ranking
// overrides.

// Count calls to the different comparison operators.
public var comparedAsComparablesCount : Int = 0
public var comparedAsFauxtingPointsCount : Int = 0

public protocol FauxtingPoint : Comparable {
  static var nan: Self { get }
  static var one: Self { get }
  static var two: Self { get }
  var bitPattern: UInt8 { get }
  @_nonoverride static func <(_ lhs: Self, _ rhs: Self) -> Bool
}

public extension FauxtingPoint {
  // This version of < will be called in a context that only knows it has a Comparable.
  @_implements(Comparable, <(_:_:))
  static func _ComparableLessThan(_ lhs: Self, _ rhs: Self) -> Bool {
    print("compared as Comparables")
    comparedAsComparablesCount += 1
    return lhs.bitPattern < rhs.bitPattern
  }
  // This version of < will be called in a context that knows it has FauxtingPoint.
  static func <(_ lhs: Self, _ rhs: Self) -> Bool {
    print("compared as FauxtingPoint")
    comparedAsFauxtingPointsCount += 1
    if lhs == Self.nan || rhs == Self.nan {
      return false
    } else {
      return lhs.bitPattern < rhs.bitPattern
    }
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

extension Fauxt: FauxtingPoint {
  // Requirement from FauxtingPoint
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
  assert(comparedAsFauxtingPointsCount == 1)
  // CHECK: compared as FauxtingPoint
  assert(!compare_FauxtingPoint(Fauxt.one, Fauxt.nan))
  assert(comparedAsFauxtingPointsCount == 2)
  // CHECK: compared as FauxtingPoint
  assert(!compare_FauxtingPoint(Fauxt.nan, Fauxt.one))
  assert(comparedAsFauxtingPointsCount == 3)
  // CHECK: compared as FauxtingPoint

  assert(compare_Fauxts(Fauxt.one, Fauxt.two))
  assert(comparedAsFauxtingPointsCount == 4)
  // CHECK: compared as FauxtingPoint
  assert(!compare_Fauxts(Fauxt.one, Fauxt.nan))
  assert(comparedAsFauxtingPointsCount == 5)
  // CHECK: compared as FauxtingPoint
  assert(!compare_Fauxts(Fauxt.nan, Fauxt.one))
  assert(comparedAsFauxtingPointsCount == 6)
  // CHECK: compared as FauxtingPoint
}
