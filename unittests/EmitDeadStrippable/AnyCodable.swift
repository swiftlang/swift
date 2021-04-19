import Foundation

/// A type-erased `Codable` value.
/// The `AnyCodable` type forwards encoding and decoding responsibilities
/// to an underlying value, hiding its specific underlying type.
/// You can encode or decode mixed-type values in dictionaries
/// and other collections that require `Encodable` or `Decodable` conformance
/// by declaring their contained type to be `AnyCodable`.

public struct AnyCodable: Codable {
  public let value: Any

  public init(_ value: Any) {
    self.value = value
  }
}

extension AnyCodable: _AnyEncodable, _AnyDecodable {
  var _encode: ((Encoder) throws -> Void)? {
    if let value = value as? Encodable {
      return value.encode
    } else {
      return nil
    }
  }
}

extension AnyCodable: CustomStringConvertible {
  public var description: String {
    switch value {
    case let value as CustomStringConvertible:
      return value.description
    default:
      return String(describing: value)
    }
  }
}

extension AnyCodable: CustomDebugStringConvertible {
  public var debugDescription: String {
    switch value {
    case let value as CustomDebugStringConvertible:
      return "AnyCodable(\(value.debugDescription))"
    default:
      return "AnyCodable(\(description))"
    }
  }
}

extension AnyCodable: Equatable {
  public static func == (lhs: AnyCodable, rhs: AnyCodable) -> Bool {
    switch (lhs.value, rhs.value) {
    case is (Void, Void):
      return true
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
    case let (lhs as [String: AnyCodable], rhs as [String: AnyCodable]):
      return lhs == rhs
    case let (lhs as [AnyCodable], rhs as [AnyCodable]):
      return lhs == rhs
    default:
      return false
    }
  }
}
