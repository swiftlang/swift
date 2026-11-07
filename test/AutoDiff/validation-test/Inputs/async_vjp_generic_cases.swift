import _Differentiation

@differentiable(reverse)
public func plainGeneric<T: Differentiable & Sendable>(
  _ x: T
) async -> T {
  return x
}

@differentiable(reverse)
public func squareGeneric<T: Differentiable & FloatingPoint & Sendable>(
  _ x: T
) async -> T where T == T.TangentVector {
  async let y = x * x
  return await y
}

@differentiable(reverse)
public func notUsefulGeneric<T: Differentiable & AdditiveArithmetic & Sendable>(
  _ x: T
) async -> T {
  async let y = x
  let z = await y
  _ = z
  return .zero
}

@differentiable(reverse, wrt: x)
public func notVariedGeneric<T: Differentiable & Sendable>(
  _ x: T, _ z: T
) async -> T {
  async let y = z
  return await y
}

@differentiable(reverse)
public func workload1Generic<T: Differentiable & AdditiveArithmetic & Sendable>(
  _ a: T, _ b: T
) async -> T where T == T.TangentVector {
  return a + b
}

@differentiable(reverse)
public func workload2Generic<T: Differentiable & FloatingPoint & Sendable>(
  _ a: T, _ b: T
) async -> T where T == T.TangentVector {
  return a * b
}

@differentiable(reverse)
public func workload3Generic<T: Differentiable & FloatingPoint & Sendable>(
  _ a: T, _ b: T, _ c: T
) async -> T where T == T.TangentVector {
  return a * b
}

@differentiable(reverse)
public func doWorkGeneric<T: Differentiable & FloatingPoint & Sendable>(
  _ a: T, _ b: T
) async -> T where T == T.TangentVector {
  async let result1 = workload1Generic(a, b)
  async let result2 = workload2Generic(a, b)
  async let result3 = workload2Generic(b, a)
  return await result1 + result2 * result3
}

@differentiable(reverse, wrt: (a, b))
public func cfgGeneric<T: Differentiable & FloatingPoint & Sendable>(
  _ a: T, _ b: T, _ flag: Bool
) async -> T where T == T.TangentVector {
  async let result1 = workload1Generic(a, b)
  async let result2 = workload2Generic(a, b)
  if flag {
    async let result3 = workload2Generic(b, a)
    return await result1 + result2 * result3
  } else {
    async let result3 = workload1Generic(b, a)
    return await result1 * result2 + result3
  }
}

@differentiable(reverse, wrt: (a, b))
public func loopGeneric<T: Differentiable & FloatingPoint & Sendable>(
  _ a: T, _ b: T, _ flag: Bool, _ n: Int
) async -> T where T == T.TangentVector {
  var sum: T = .zero
  for i in 0...n {
    async let result0 = workload1Generic(T(i) * a, b)
    sum += await result0
  }
  async let result1 = workload1Generic(a, b)
  async let result2 = workload2Generic(a, b)
  if flag {
    async let result3 = workload2Generic(b, a)
    return await sum * result1 + result2 * result3
  } else {
    async let result3 = workload1Generic(b, a)
    return await sum + result1 * result2 + result3
  }
}

@differentiable(reverse, wrt: (a, b))
public func ternaryGeneric<T: Differentiable & FloatingPoint & Sendable>(
  _ a: T, _ b: T, _ flag: Bool, _ n: Int
) async -> T where T == T.TangentVector {
  var sum: T = .zero
  for i in 0...n {
    async let result0 = workload1Generic(T(i) * a, b)
    sum += await result0
  }
  async let result1 = workload1Generic(a, b)
  async let result2 = workload3Generic(a, b, a)
  if flag {
    async let result3 = workload3Generic(b, a, b)
    return await sum * result1 + result2 * result3
  } else {
    async let result3 = workload1Generic(b, a)
    return await sum + result1 * result2 + result3
  }
}

#if canImport(Glibc) || canImport(Darwin) || canImport(Musl) || canImport(Android)

@differentiable(reverse)
public func sleepWorkload1Generic<T: Differentiable & AdditiveArithmetic & Sendable>(
  _ a: T, _ b: T
) async -> T where T == T.TangentVector {
  asyncVJPBlockingSleep(milliseconds: 1)
  return a + b
}

@differentiable(reverse)
public func sleepWorkload2Generic<T: Differentiable & FloatingPoint & Sendable>(
  _ a: T, _ b: T
) async -> T where T == T.TangentVector {
  asyncVJPBlockingSleep(milliseconds: 1)
  return a * b
}

@differentiable(reverse)
public func sleepWorkGeneric<T: Differentiable & FloatingPoint & Sendable>(
  _ a: T, _ b: T
) async -> T where T == T.TangentVector {
  async let result1 = sleepWorkload1Generic(a, b)
  async let result2 = sleepWorkload2Generic(a, b)
  return await result1 + result2
}

#endif

@differentiable(reverse)
public func threeCapturesGeneric<T: Differentiable & FloatingPoint & Sendable>(
  _ a: T, _ b: T, _ c: T
) async -> T where T == T.TangentVector {
  async let y = workload3Generic(a, b, c)
  return await y
}

@differentiable(reverse)
public func identityChildGeneric<T: Differentiable & Sendable>(
  _ x: T
) async -> T {
  async let y = x
  return await y
}
