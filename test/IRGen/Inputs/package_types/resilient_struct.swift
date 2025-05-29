package struct Point {
  package var x: Int // read-write stored property
  package let y: Int // read-only stored property

  package init(x: Int, y: Int) {
    self.x = x
    self.y = y
  }

  package func method() {}
  package mutating func mutantMethod() {}
}

package struct Size {
  package var w: Int // should have getter and setter
  package let h: Int // getter only

  package init(w: Int, h: Int) {
    self.w = w
    self.h = h
  }

  package func method() {}
  package mutating func mutantMethod() {}
}

package struct Rectangle {
  package let p: Point
  package let s: Size
  package let color: Int

  package init(p: Point, s: Size, color: Int) {
    self.p = p
    self.s = s
    self.color = color
  }
}

// More complicated resilient structs for runtime tests
package struct ResilientBool {
  package let b: Bool

  package init(b: Bool) {
    self.b = b
  }
}

package struct ResilientInt {
  package let i: Int

  package init(i: Int) {
    self.i = i
  }
}

package struct ResilientDouble {
  package let d: Double

  package init(d: Double) {
    self.d = d
  }
}

package struct ResilientLayoutRuntimeTest {
  package let b1: ResilientBool
  package let i: ResilientInt
  package let b2: ResilientBool
  package let d: ResilientDouble

  package init(b1: ResilientBool, i: ResilientInt, b2: ResilientBool, d: ResilientDouble) {
    self.b1 = b1
    self.i = i
    self.b2 = b2
    self.d = d
  }
}

package class Referent {
  package init() {}
}

package struct ResilientWeakRef {
  package weak var ref: Referent?

  package init (_ r: Referent) {
    ref = r
  }
}

package struct ResilientRef {
  package var r: Referent

  package init(r: Referent) { self.r = r }
}

package struct ResilientWithInternalField {
  var x: Int
}

// Tuple parameters with resilient structs
package class Subject {}

package struct Container {
  package var s: Subject
}

package struct PairContainer {
  package var pair : (Container, Container)
}

@available(*, unavailable)
package struct UnavailableResilientInt {
  package let i: Int

  package init(i: Int) {
    self.i = i
  }
}


public struct PublicPoint {
  public var x: Int // read-write stored property
  public let y: Int // read-only stored property

  public init(x: Int, y: Int) {
    self.x = x
    self.y = y
  }

  public func method() {}
  public mutating func mutantMethod() {}
}

@frozen
public struct FrozenPublicPoint {
  public var x: Int // read-write stored property
  public let y: Int // read-only stored property

  public init(x: Int, y: Int) {
    self.x = x
    self.y = y
  }

  public func method() {}
  public mutating func mutantMethod() {}
}

public struct PublicSize {
  public var w: Int // should have getter and setter
  public let h: Int // getter only

  public init(w: Int, h: Int) {
    self.w = w
    self.h = h
  }

  public func method() {}
  public mutating func mutantMethod() {}
}

@frozen
public struct FrozenPublicSize {
  public var w: Int // should have getter and setter
  public let h: Int // getter only

  public init(w: Int, h: Int) {
    self.w = w
    self.h = h
  }

  public func method() {}
  public mutating func mutantMethod() {}
}

public struct PublicResilientInt {
  public let i: Int

  public init(i: Int) {
    self.i = i
  }
}
