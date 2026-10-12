// RUN: %target-swift-frontend -emit-sil -sil-ownership-verify-all -O %s

// Ensure that we do correct ownership of VJP normal result

import _Differentiation

enum E: Error {
  case error
}

@differentiable(reverse)
public func f(x: Double) throws -> Double {
  if x < 0 {
      throw E.error
  } else {
      return x * x
  }
}

@differentiable(reverse)
public func g(x: Double) -> Double {
  do {
    return try f(x: x)
  } catch {
    return 2*x
  }
}
