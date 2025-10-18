protocol MagicStringInterpolationProtocol: StringInterpolationProtocol {}

extension MagicStringInterpolationProtocol {
  mutating func appendInterpolation<Value>(_ value: Value) {}
}

