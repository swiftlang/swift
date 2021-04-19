import Foundation

/// A type-erased `Encodable` value.

/// The `AnyEncodable` type forwards encoding responsibilities
/// to an underlying value, hiding its specific underlying type.

/// You can encode mixed-type values in dictionaries
/// and other collections that require `Encodable` conformance
/// by declaring their contained type to be `AnyEncodable`

public struct AnyEncodable: Encodable {
  public let value: Any
  let _encode: ((Encoder) throws -> Void)?

  public init(_ value: Any) {
    self.value = value
    if let value = value as? Encodable {
      self._encode = value.encode
    } else {
      self._encode = nil
    }
  }
}

// Provides default implementation of `Encodable` protocol for the forwarded `value`.
protocol _AnyEncodable {
  var value: Any { get }
  var _encode: ((Encoder) throws -> Void)? { get }

  init(_ value: Any)
}

extension AnyEncodable: _AnyEncodable {}

// MARK: - Encodable

extension _AnyEncodable {
  public func encode(to encoder: Encoder) throws {
    if value is Encodable {
      try _encode?(encoder)
    } else {
      var container = encoder.singleValueContainer()

      switch value {
      case let bool as Bool:
        try container.encode(bool)
      case let int as Int:
        try container.encode(int)
      case let int8 as Int8:
        try container.encode(int8)
      case let int16 as Int16:
        try container.encode(int16)
      case let int32 as Int32:
        try container.encode(int32)
      case let int64 as Int64:
        try container.encode(int64)
      case let uint as UInt:
        try container.encode(uint)
      case let uint8 as UInt8:
        try container.encode(uint8)
      case let uint16 as UInt16:
        try container.encode(uint16)
      case let uint32 as UInt32:
        try container.encode(uint32)
      case let uint64 as UInt64:
        try container.encode(uint64)
      case let float as Float:
        try container.encode(float)
      case let double as Double:
        try container.encode(double)
      case let string as String:
        try container.encode(string)
      case let date as Date:
        try container.encode(date)
      case let url as URL:
        try container.encode(url)
      case let array as [Any?]:
        try container.encode(array.map { AnyCodable($0 as Any) })
      case let dictionary as [String: Any?]:
        try container.encode(dictionary.mapValues { AnyCodable($0 as Any) })
      default:
        let context = EncodingError.Context(codingPath: container.codingPath, debugDescription: "AnyEncodable value cannot be encoded")
        throw EncodingError.invalidValue(value, context)
      }
    }
  }
}

extension AnyEncodable: CustomStringConvertible {
  public var description: String {
    switch value {
    case let value as CustomStringConvertible:
      return value.description
    default:
      return String(describing: value)
    }
  }
}

extension AnyEncodable: CustomDebugStringConvertible {
  public var debugDescription: String {
    switch value {
    case let value as CustomDebugStringConvertible:
      return "AnyEncodable(\(value.debugDescription))"
    default:
      return "AnyEncodable(\(description))"
    }
  }
}

extension AnyEncodable: Equatable {
  public static func == (lhs: AnyEncodable, rhs: AnyEncodable) -> Bool {
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
    case let (lhs as [String: AnyEncodable], rhs as [String: AnyEncodable]):
      return lhs == rhs
    case let (lhs as [AnyEncodable], rhs as [AnyEncodable]):
      return lhs == rhs
    default:
      return false
    }
  }
}
