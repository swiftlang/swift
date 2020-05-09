public protocol P {}

public struct M<T : P> : P {
  public init(t: T) {}
}
extension Int : P {}

extension P {
 @inlinable
 public func o<T : P>(_ t: T) -> some P {
   return M<T>(t: t)
 }

 @inlinable
 public func p() throws -> some P {
   return Int()
 }
}
