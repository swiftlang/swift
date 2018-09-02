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

public enum MyStringError: Error {
  case failure
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
  
  public init(stringInterpolation: StringInterpolation) {
    self.init(str: stringInterpolation.result)
  }
  
  public struct StringInterpolation: StringInterpolationProtocol {
    var result: String
    
    public init(literalCapacity: Int, interpolationCount: Int) {
      result = String(literalCapacity) + "/" + String(interpolationCount)
    }
    
    public mutating func appendLiteral(_ literal: String) {
      result += "<literal " + literal + ">"
    }
    
    public mutating func appendInterpolation<T>(_ expr: T) {
      result += "<interpolation:T " + String(describing: expr) + ">"
    }
    
    public mutating func appendInterpolation(_ expr: Int) {
      result += "<interpolation:Int " + String(expr) + ">"
    }
    
    public mutating func appendInterpolation(_ expr: Int, radix: Int) {
      result += "<interpolation:Int,radix " + String(expr, radix: radix) + ">"
    }
    
    public mutating func appendInterpolation<T>(debug: T) {
      result += "<interpolation:T debug: " + String(reflecting: debug) + ">"
    }
    
    public mutating func appendInterpolation(fails: Bool) throws {
      if fails {
        throw MyStringError.failure
      }
      result += "<interpolation:fails >"
    }
    
    public mutating func appendInterpolation(required: Bool, optional: Bool = false) {
      result += "<interpolation:required:optional " + String(reflecting: required) + " " + String(reflecting: optional) + ">"
    }
  }
}

public struct MySimpleString : ExpressibleByStringInterpolation {
  public var value: String
  
  public init(stringLiteral: String) {
    value = stringLiteral
  }
}
