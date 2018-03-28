var __lolcode_IT: Any = 0

public func _set_lolcode_IT(_ value: Any) {
  __lolcode_IT = value
}

public func _get_lolcode_IT() -> Any {
  return __lolcode_IT
}

public enum _LOLCodeType: String {
  case yarn, numbar, numbr, noob, bukkit, troof

  var defaultValue: _LOLCodeValue {
    switch self {
    case .bukkit: return .bukkit([])
    case .noob: return .noob
    case .numbar: return .numbar(0.0)
    case .numbr: return .numbr(0)
    case .troof: return .troof(false)
    case .yarn: return .yarn("")
    }
  }
}

public enum _LOLCodeValue: Equatable {
  case yarn(String)
  case numbar(Double)
  case numbr(Int)
  case noob
  case bukkit([_LOLCodeValue])
  case troof(Bool)

  public static func == (lhs: _LOLCodeValue, rhs: _LOLCodeValue) -> Bool {
    switch (lhs, rhs) {
    case let (.yarn(l), .yarn(r)): return l == r
    case let (.numbar(l), .numbar(r)): return l == r
    case let (.numbr(l), .numbr(r)): return l == r
    case let (.numbar(l), .numbr(r)): return l == Double(r)
    case let (.numbr(l), .numbar(r)): return Double(l) == r
    case (.noob, .noob): return true
    case let (.bukkit(l), .bukkit(r)): return l == r
    case let (.troof(l), .troof(r)): return l == r
    default: return false
    }
  }

  var _type: _LOLCodeType {
    switch self {
    case .yarn(_):
      return .yarn
    case .numbar(_):
      return .numbar
    case .numbr(_):
      return .numbr
    case .noob:
      return .noob
    case .bukkit(_):
      return .bukkit
    case .troof(_):
      return .troof
    }
  }

  var value: Any {
    switch self {
    case .yarn(let s): return s
    case .numbar(let d): return d
    case .numbr(let i): return i
    case .noob: return ()
    case .bukkit(let arr): return arr.map { $0.value }
    case .troof(let b): return b
    }
  }

  func cast(to type: _LOLCodeType) -> _LOLCodeValue {
    guard self._type != type else {
      return self
    }

    switch (self, type) {
    case (_, .troof): return .troof(asTroof)
    case (_, .yarn): return .yarn("\(self.value)")
    case (.noob, let ty): return ty.defaultValue
    case let (.numbr(n), .numbar): return .numbar(Double(n))
    case let (.numbar(d), .numbr): return .numbr(Int(d))
    case let (.yarn(s), .numbar):
      guard let d = Double(s) else { break }
      return .numbar(d)
    case let (.yarn(s), .numbr):
      guard let i = Int(s, radix: 10) else { break }
      return .numbr(i)
    case let (.troof(b), .numbr): return .numbr(b ? 1 : 0)
    case let (.troof(b), .numbar): return .numbar(b ? 1 : 0)
    default: break
    }
    fatalError("i cant maek '\(self)' in2 a '\(type.rawValue)'")
  }

  var asTroof: Bool {
    switch self {
    case .yarn(let s):
      return s.contains("t") || s.contains("y") || s.contains("1")
    case .numbar(let n): return n != 0
    case .numbr(let n): return n != 0
    case .troof(let b): return b
    case .noob, .bukkit: return false
    }
  }

  init(_ value: Any) {
    switch value {
    case is Void:
      self = .noob
    case let value as _LOLCodeValue:
      self = value
    case let value as String:
      self = .yarn(value)
    case let value as Int:
      self = .numbr(value)
    case let value as Double:
      self = .numbar(value)
    case let value as Bool:
      self = .troof(value)
    case let value as [Any]:
      self = .bukkit(value.map(_LOLCodeValue.init))
    case let value as UInt8:
      self = .numbr(numericCast(value))
    case let value as UInt16:
      self = .numbr(numericCast(value))
    case let value as UInt32:
      self = .numbr(numericCast(value))
    case let value as UInt64:
      self = .numbr(numericCast(value))
    case let value as Int8:
      self = .numbr(numericCast(value))
    case let value as Int16:
      self = .numbr(numericCast(value))
    case let value as Int32:
      self = .numbr(numericCast(value))
    case let value as Int64:
      self = .numbr(numericCast(value))
    case let value as UInt:
      self = .numbr(numericCast(value))
    case let value as Float:
      self = .numbar(Double(value))
    case let value as Float80:
      self = .numbar(Double(value))
    default:
      self = .yarn("\(value)")
    }
  }
}

public func _lolcode_promoteNumericPair(
  _ lhs: _LOLCodeValue,
  _ rhs: _LOLCodeValue,
  _ operation: String
  ) -> (_LOLCodeValue, _LOLCodeValue) {
  switch (lhs, rhs) {
  case (.numbar, .numbar), (.numbr, .numbr):
    return (lhs, rhs)
  // Promote (int, fp) and (fp, int) to (fp, fp)
  case (.numbar, .numbr):
    return (lhs, rhs.cast(to: .numbar))
  case (.numbr, .numbar):
    return (lhs.cast(to: .numbar), rhs)

  // Promote (bool, int) and (int, bool) to (int, int)
  case (.numbr, .troof):
    return (lhs, rhs.cast(to: .numbr))
  case (.troof, .numbr):
    return (lhs.cast(to: .numbr), rhs)

  // Promote (bool, fp) and (fp, bool) to (fp, fp)
  case (.troof, .numbar):
    return (lhs.cast(to: .numbar), rhs)
  case (.numbar, .troof):
    return (lhs, rhs.cast(to: .numbr))

  // promote (str, int or fp) and (int or fp, str) to (int or fp, int or fp)
  case (.yarn(let s), _):
    if let int = Int(s, radix: 10) {
      return _lolcode_promoteNumericPair(.numbr(int), rhs, operation)
    }
    if let double = Double(s) {
      return _lolcode_promoteNumericPair(.numbar(double), rhs, operation)
    }
  case (_, .yarn(let s)):
    if let int = Int(s, radix: 10) {
      return _lolcode_promoteNumericPair(lhs, .numbr(int), operation)
    }
    if let double = Double(s) {
      return _lolcode_promoteNumericPair(lhs, .numbar(double), operation)
    }
  default: break
  }
  fatalError("i dunno how 2 \(operation) '\(lhs.value)' an '\(rhs.value)'")
}

public func _lolcode_binary_op(
  _ lhs: Any,
  _ rhs: Any,
  _ operation: String,
  numbarOp: (Double, Double) -> Double,
  numbrOp: (Int, Int) -> Int
  ) -> Any {
  let (lhsVal, rhsVal) = _lolcode_promoteNumericPair(
    _LOLCodeValue(lhs),
    _LOLCodeValue(rhs),
    operation
  )
  switch (lhsVal, rhsVal) {
  case (.numbar(let f1), .numbar(let f2)):
    return numbarOp(f1, f2)
  case (.numbr(let n1), .numbr(let n2)):
    return numbrOp(n1, n2)
  default:
    fatalError("""
      oh noes! numeric pair iz wrong!1!! \
      \(type(of: lhsVal)) and \(type(of: rhsVal))
      """)
  }
}

/// MARK: Math functions

public func _lolcode_sum(_ lhs: Any, _ rhs: Any) -> Any {
  return _lolcode_binary_op(lhs, rhs, "add",
                            numbarOp: +, numbrOp: +)
}

public func _lolcode_diff(_ lhs: Any, _ rhs: Any) -> Any {
  return _lolcode_binary_op(lhs, rhs, "subtract",
                            numbarOp: -, numbrOp: -)
}

public func _lolcode_produkt(_ lhs: Any, _ rhs: Any) -> Any {
  return _lolcode_binary_op(lhs, rhs, "multiply",
                            numbarOp: *, numbrOp: *)
}

public func _lolcode_quoshunt(_ lhs: Any, _ rhs: Any) -> Any {
  return _lolcode_binary_op(lhs, rhs, "divide",
                            numbarOp: /, numbrOp: /)
}

public func _lolcode_mod(_ lhs: Any, _ rhs: Any) -> Any {
  return _lolcode_binary_op(lhs, rhs, "get remainder of",
                            numbarOp: {
                              $0.truncatingRemainder(dividingBy: $1)
  },
                            numbrOp: %)
}

public func _lolcode_biggr(_ lhs: Any, _ rhs: Any) -> Any {
  return _lolcode_binary_op(lhs, rhs, "get bigger of",
                            numbarOp: max, numbrOp: max)
}

public func _lolcode_smallr(_ lhs: Any, _ rhs: Any) -> Any {
  return _lolcode_binary_op(lhs, rhs, "get smaller of",
                            numbarOp: min, numbrOp: min)
}

/// MARK: Boolean operators

public func _lolcode_both_of(_ lhs: Any, _ rhs: Any) -> Any {
  return _LOLCodeValue(lhs).asTroof && _LOLCodeValue(rhs).asTroof
}

public func _lolcode_either(_ lhs: Any, _ rhs: Any) -> Any {
  return _LOLCodeValue(lhs).asTroof || _LOLCodeValue(rhs).asTroof
}

public func _lolcode_won_of(_ lhs: Any, _ rhs: Any) -> Any {
  return _LOLCodeValue(lhs).asTroof != _LOLCodeValue(rhs).asTroof
}

public func _lolcode_not(_ value: Any) -> Any {
  return !_LOLCodeValue(value).asTroof
}

public func _lolcode_all_of(_ values: Any...) -> Any {
  for value in values {
    if !_LOLCodeValue(value).asTroof { return false }
  }
  return true
}

public func _lolcode_any_of(_ values: Any...) -> Any {
  for value in values {
    if _LOLCodeValue(value).asTroof { return true }
  }
  return false
}


/// MARK: Comparison operators

public func _lolcode_both_saem(_ lhs: Any, _ rhs: Any) -> Any {
  return _LOLCodeValue(lhs) == _LOLCodeValue(rhs)
}

public func _lolcode_diffrint(_ lhs: Any, _ rhs: Any) -> Any {
  return _LOLCodeValue(lhs) != _LOLCodeValue(rhs)
}

/// MARK: Misc. operators

public func _lolcode_smoosh(_ values: Any...) -> Any {
  return values.map { "\($0)" }.joined() as String
}

public func _lolcode_visible(_ values: Any..., newline: Bool) {
  for value in values {
    print(_LOLCodeValue(value).value, terminator: "")
  }
  if newline {
    print()
  }
}

public func _lolcode_gimmeh() -> Any {
  guard let value = readLine() else { return "" }
  return value
}

