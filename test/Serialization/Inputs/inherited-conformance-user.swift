import Base
import Lib

// Instantiate Counter<Int>, relying on Counter's adoption of SimpleProto.
public struct OneToAThousand : ProtoUser {
  public typealias Impl = Counter<Int>

  public var start: Impl {
    return Impl(value: 1)
  }

  public var end: Impl {
    return Impl(value: 1001)
  }

  public subscript(i: Impl) -> Int {
    return i.value
  }

  public init() {}
}

public protocol SpecialProto : ExpressibleByIntegerLiteral {}
extension Int : SpecialProto {}

// Subclass a class with private conformances.
open class SubclassConformsToPrivateProto : ConformsToPrivateProto {}
