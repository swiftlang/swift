import Foundation

/// A type-erased `Decodable` value.

/// The `AnyDecodable` type forwards decoding responsibilities
/// to an underlying value, hiding its specific underlying type.

/// You can decode mixed-type values in dictionaries
/// and other collections that require `Decodable` conformance
/// by declaring their contained type to be `AnyDecodable`

public struct AnyDecodable: Decodable {
  public let value: Any

  public init(_ value: Any) {
    self.value = value
  }
}

// Provides default implementation of `Decodable` to its forwarded value.
protocol _AnyDecodable {
  var value: Any { get }
  init(_ value: Any)
}

extension AnyDecodable: _AnyDecodable {}

extension _AnyDecodable {
  public init(from decoder: Decoder) throws {
    let container = try decoder.singleValueContainer()

    if let bool = try? container.decode(Bool.self) {
      self.init(bool)
    } else if let int = try? container.decode(Int.self) {
      self.init(int)
    } else if let uint = try? container.decode(UInt.self) {
      self.init(uint)
    } else if let double = try? container.decode(Double.self) {
      self.init(double)
    } else if let string = try? container.decode(String.self) {
      self.init(string)
    } else if let array = try? container.decode([AnyCodable].self) {
      self.init(array.map { $0.value })
    } else if let dictionary = try? container.decode([String: AnyCodable].self) {
      self.init(dictionary.mapValues { $0.value })
    } else {
      throw DecodingError.dataCorruptedError(in: container, debugDescription: "AnyCodable value cannot be decoded")
    }
  }
}

extension AnyDecodable: CustomStringConvertible {
  public var description: String {
    switch value {
    case let value as CustomStringConvertible:
      return value.description
    default:
      return String(describing: value)
    }
  }
}

extension AnyDecodable: CustomDebugStringConvertible {
  public var debugDescription: String {
    switch value {
    case let value as CustomDebugStringConvertible:
      return "AnyDecodable(\(value.debugDescription))"
    default:
      return "AnyDecodable(\(description))"
    }
  }
}

extension AnyDecodable: Equatable {
  public static func == (lhs: AnyDecodable, rhs: AnyDecodable) -> Bool {
    switch (lhs.value, rhs.value) {
    case let (lhs as Bool, rhs as Bool):
      return lhs == rhs
    case let (lhs as Int, rhs as Int):
      return lhs == rhs
    case let (lhs as Int8, rhs as Int8):
      return lhs == rhs
    case let (lhs as Int16, rhs as Int16):
      return lhs == rhs
    case let (lhs as Int32, rhs as Int32):
      return lhs == rhs
    case let (lhs as Int64, rhs as Int64):
      return lhs == rhs
    case let (lhs as UInt, rhs as UInt):
      return lhs == rhs
    case let (lhs as UInt8, rhs as UInt8):
      return lhs == rhs
    case let (lhs as UInt16, rhs as UInt16):
      return lhs == rhs
    case let (lhs as UInt32, rhs as UInt32):
      return lhs == rhs
    case let (lhs as UInt64, rhs as UInt64):
      return lhs == rhs
    case let (lhs as Float, rhs as Float):
      return lhs == rhs
    case let (lhs as Double, rhs as Double):
      return lhs == rhs
    case let (lhs as String, rhs as String):
      return lhs == rhs
    case let (lhs as [String: AnyDecodable], rhs as [String: AnyDecodable]):
      return lhs == rhs
    case let (lhs as [AnyDecodable], rhs as [AnyDecodable]):
      return lhs == rhs
    default:
      return false
    }
  }
}
