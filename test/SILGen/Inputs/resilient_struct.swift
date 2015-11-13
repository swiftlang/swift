// Fixed-layout struct
@_fixed_layout public struct Point {
  public var x: Int // read-write stored property
  public let y: Int // read-only stored property

  public init(x: Int, y: Int) {
    self.x = x
    self.y = y
  }

  public func method() {}
  public mutating func mutantMethod() {}
}

// Resilient-layout struct
public struct Size {
  public var w: Int // should have getter and setter
  public let h: Int // getter only

  public init(w: Int, h: Int) {
    self.w = w
    self.h = h
  }

  public func method() {}
  public mutating func mutantMethod() {}
}

// Fixed-layout struct with resilient-layout members
@_fixed_layout public struct Rectangle {
  public let p: Point
  public let s: Size

  public init(p: Point, s: Size) {
    self.p = p
    self.s = s
  }
}
