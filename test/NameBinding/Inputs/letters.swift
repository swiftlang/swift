@exported import abcde
@exported import aeiou
@exported import struct aeiou.E
@exported import struct asdf.D
import struct asdf.S

public struct C {
  public var b : B
  public init(b: B) { self.b = b }
}

public struct F { public init() {} }

public var myS : S = S()
public func consumeS(s: S) {}

