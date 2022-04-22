public func with_defaults<T>(_ v: T = 42.0) {
  print("with_defaults: \(v); type = \(type(of: v))")
}
