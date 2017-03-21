public protocol ProtocolUnrelatedToPrinting {}

public struct StructPrintable : CustomStringConvertible,
  ProtocolUnrelatedToPrinting {

  let x: Int

  public init(_ x: Int) {
    self.x = x
  }

  public var description: String {
    return "►\(x)◀︎"
  }
}

public struct LargeStructPrintable : CustomStringConvertible,
  ProtocolUnrelatedToPrinting {

  let a: Int
  let b: Int
  let c: Int
  let d: Int

  public init(_ a: Int, _ b: Int, _ c: Int, _ d: Int) {
    self.a = a
    self.b = b
    self.c = c
    self.d = d
  }

  public var description: String {
    return "<\(a) \(b) \(c) \(d)>"
  }
}

public struct StructDebugPrintable : CustomDebugStringConvertible {
  let x: Int

  public init(_ x: Int) {
    self.x = x
  }

  public var debugDescription: String {
    return "►\(x)◀︎"
  }
}

public struct StructVeryPrintable : CustomStringConvertible,
  CustomDebugStringConvertible, ProtocolUnrelatedToPrinting {

  let x: Int

  public init(_ x: Int) {
    self.x = x
  }

  public var description: String {
    return "<description: \(x)>"
  }

  public var debugDescription: String {
    return "<debugDescription: \(x)>"
  }
}

public struct EmptyStructWithoutDescription {
  public init() {}
}

public struct WithoutDescription {
  let x: Int

  public init(_ x: Int) {
    self.x = x
  }
}

public struct ValuesWithoutDescription<T, U, V> {
  let t: T
  let u: U
  let v: V

  public init(_ t: T, _ u: U, _ v: V) {
    self.t = t
    self.u = u
    self.v = v
  }
}


public class ClassPrintable : CustomStringConvertible,
  ProtocolUnrelatedToPrinting {

  let x: Int

  public init(_ x: Int) {
    self.x = x
  }

  public var description: String {
    return "►\(x)◀︎"
  }
}

public class ClassVeryPrintable : CustomStringConvertible,
  CustomDebugStringConvertible, ProtocolUnrelatedToPrinting {

  let x: Int

  public init(_ x: Int) {
    self.x = x
  }

  public var description: String {
    return "<description: \(x)>"
  }

  public var debugDescription: String {
    return "<debugDescription: \(x)>"
  }
}

public struct MyString : ExpressibleByStringLiteral,
  ExpressibleByStringInterpolation {

  public init(str: String) {
    value = str
  }

  public var value: String

  public init(unicodeScalarLiteral value: String) {
    self.init(str: value)
  }

  public init(extendedGraphemeClusterLiteral value: String) {
    self.init(str: value)
  }

  public init(stringLiteral value: String) {
    self.init(str: value)
  }
  
  public init(stringLiteral segments: StringInterpolationSegment<String, String>...) {
    var result = ""
    for s in segments {
      switch s {
      case .stringLiteral(let str):
        result += str
      case .stringInterpolation(let str):
        result += "<segment " + String(describing: str) + ">"
      }
    }
    self.init(str: result)
  }
}

