@available(SwiftCompatibilitySpan 5.0, *)
public func takeSpan(_ s: Span<Int>) -> Int {
  s.isEmpty ? 0 : s[0]
}

public func noSpan(_ x: Int) -> Int {
  x + 1
}
