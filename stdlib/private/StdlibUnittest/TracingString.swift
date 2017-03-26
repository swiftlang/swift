/// A string type which records the initializers called upon it and 
/// compares equal if the calls were identical.
public struct TracingString {
  enum Initializer {
    case stringInterpolation([StringInterpolationSegment<String, TracingString.Interpolation>])
    case stringLiteral(String)
    case unicodeScalarLiteral(UnicodeScalar)
    case extendedGraphemeClusterLiteral(Character)
  }
  var initializer: Initializer
  
  public struct Interpolation {
    enum Initializer {
      case forInterpolation(AnyHashable)
      case _WithInteger(AnyHashable, radix: Int, uppercase: Bool)
    }
    var initializer: Initializer
  }
}

extension TracingString: ExpressibleByStringInterpolation {
  public init(stringInterpolation segments: StringInterpolationSegment<String, Interpolation>...) {
    initializer = .stringInterpolation(segments)
  }
  
  public init(stringLiteral value: String) {
    initializer = .stringLiteral(value)
  }

  public init(unicodeScalarLiteral value: UnicodeScalar) {
    initializer = .unicodeScalarLiteral(value)
  }

  public init(extendedGraphemeClusterLiteral value: Character) {
    initializer = .extendedGraphemeClusterLiteral(value)
  }
}

extension TracingString.Interpolation {
  public init<T: Hashable>(forInterpolation value: T) {
    initializer = .forInterpolation(value)
  }
  
  public init(_ value: Int, radix: Int = 10, uppercase: Bool = false) {
    initializer = ._WithInteger(value, radix: radix, uppercase: uppercase)
  }
}

fileprivate func hashable(for segment: StringInterpolationSegment<String, TracingString.Interpolation>) -> AnyHashable {
  switch segment {
  case .literal(let string):
    return string
  case .interpolation(let interpolation):
    return interpolation
  }
}

extension TracingString: Hashable {
  public static func == (lhs: TracingString, rhs: TracingString) -> Bool {
    switch (lhs.initializer, rhs.initializer) {
    case let (.stringLiteral(l), .stringLiteral(r)):
      return l == r
    case let (.unicodeScalarLiteral(l), .unicodeScalarLiteral(r)):
      return l == r
    case let (.extendedGraphemeClusterLiteral(l), .extendedGraphemeClusterLiteral(r)):
      return l == r
    case let (.stringInterpolation(l), .stringInterpolation(r)):
      return l.map(hashable(for:)) == r.map(hashable(for:))
    case (.stringLiteral, _),
        (.unicodeScalarLiteral, _),
        (.extendedGraphemeClusterLiteral, _),
        (.stringInterpolation, _):
        return false
    }
  }
  
  public var hashValue: Int {
    switch initializer {
    case .stringLiteral(let value):
      return value.hashValue
    case .unicodeScalarLiteral(let value):
      return value.hashValue
    case .extendedGraphemeClusterLiteral(let value):
      return value.hashValue
    case .stringInterpolation(let segments):
      return segments.reduce(segments.count) { $0 ^ hashable(for: $1).hashValue }
    }
  }
}

extension TracingString.Interpolation: Hashable {
  public static func == (lhs: TracingString.Interpolation, rhs: TracingString.Interpolation) -> Bool {
    switch (lhs.initializer, rhs.initializer) {
    case let (.forInterpolation(l), .forInterpolation(r)):
      return l == r
    case let (._WithInteger(lValue, radix: lRadix, uppercase: lUppercase), ._WithInteger(rValue, radix: rRadix, uppercase: rUppercase)):
      return (lValue, lRadix, lUppercase) == (rValue, rRadix, rUppercase)
    case (.forInterpolation, _),
          (._WithInteger, _):
      return false
    }
  }
  
  public var hashValue: Int {
    switch initializer {
    case let .forInterpolation(value):
      return value.hashValue
    case let ._WithInteger(value, radix: radix, uppercase: uppercase):
      return value.hashValue ^ ~radix.hashValue ^ uppercase.hashValue
    }
  }
}

extension StringInterpolationSegment {
  fileprivate var dumped: String {
    switch self {
    case .literal(let str):
      return "    .literal(\(reflecting: str))"
    case .interpolation(let expr):
      return "    .interpolation(\(reflecting: expr))"
    }
  }
}

extension TracingString: CustomDebugStringConvertible {
  public var debugDescription: String {
    switch initializer {
    case .stringLiteral(let value):
      return "TracingString(stringLiteral: \(reflecting: value))"
    case .unicodeScalarLiteral(let value):
      return "TracingString(unicodeScalarLiteral: \(reflecting: value))"
    case .extendedGraphemeClusterLiteral(let value):
      return "TracingString(extendedGraphemeClusterLiteral: \(reflecting: value))"
    case .stringInterpolation(let segments):
      let segmentsString = segments.map { $0.dumped }.joined(separator: ",\n")
      return "TracingString(stringInterpolation: \n\(segmentsString)\n)"
    }
  }
}

extension TracingString.Interpolation: CustomDebugStringConvertible {
  public var debugDescription: String {
    switch initializer {
    case .forInterpolation(let value):
      return ".init(forInterpolation: \(reflecting: value))"
    case let ._WithInteger(value, radix: radix, uppercase: uppercase):
      // Strip off the AnyHashable() wrapper
      let valueDesc = String(String(reflecting: value).characters.dropFirst(12).dropLast())
      return ".init(\(valueDesc), radix: \(reflecting: radix), uppercase: \(reflecting: uppercase))"
    }
  }
}