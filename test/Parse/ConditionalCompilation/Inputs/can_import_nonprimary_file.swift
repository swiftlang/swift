#if canImport(Swift)
public func performFoo(withX x: Int, andY y: Int) -> Int {
#if canImport(Darwin)
  return x + y
#else
  return x * y
#endif
}
#endif

