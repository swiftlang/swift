public struct BitwiseStruct<T> {
  var t: T

  public init(t: T) { self.t = t }
}

extension BitwiseStruct: BitwiseCopyable where T: BitwiseCopyable {}

public enum BitwiseEnum<T> {
  case t(T)
}

extension BitwiseEnum: BitwiseCopyable where T: BitwiseCopyable {}
