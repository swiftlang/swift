import _Differentiation

// plain.swift: no task, an ordinary async VJP baseline.

@differentiable(reverse)
public func plainGeneric<T: Differentiable & Sendable>(
  _ x: T
) async -> T {
  return x
}

// src.swift: one task; both occurrences of x contribute.

@differentiable(reverse)
public func squareGeneric<T: Differentiable & FloatingPoint & Sendable>(
  _ x: T
) async -> T where T == T.TangentVector {
  async let y = x * x
  return await y
}

// notUseful.swift: varied child result, unused by the return.

@differentiable(reverse)
public func notUsefulGeneric<T: Differentiable & AdditiveArithmetic & Sendable>(
  _ x: T
) async -> T {
  async let y = x
  let z = await y
  _ = z
  return .zero
}

// notVaried.swift: only x is selected; the child depends on z.

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

// ternary.swift deliberately does not use c.

@differentiable(reverse)
public func workload3Generic<T: Differentiable & FloatingPoint & Sendable>(
  _ a: T, _ b: T, _ c: T
) async -> T where T == T.TangentVector {
  return a * b
}

// dowork.swift / generic.swift: the third capture order is b, a.

@differentiable(reverse)
public func doWorkGeneric<T: Differentiable & FloatingPoint & Sendable>(
  _ a: T, _ b: T
) async -> T where T == T.TangentVector {
  async let result1 = workload1Generic(a, b)
  async let result2 = workload2Generic(a, b)
  async let result3 = workload2Generic(b, a)
  return await result1 + result2 * result3
}

// dowork-sync.swift: ordinary synchronous control, not the oracle.

@differentiable(reverse)
public func syncWorkload1Generic<T: Differentiable & AdditiveArithmetic & Sendable>(
  _ a: T, _ b: T
) -> T where T == T.TangentVector {
  return a + b
}

@differentiable(reverse)
public func syncWorkload2Generic<T: Differentiable & FloatingPoint & Sendable>(
  _ a: T, _ b: T
) -> T where T == T.TangentVector {
  return a * b
}

@differentiable(reverse)
public func doWorkSyncGeneric<T: Differentiable & FloatingPoint & Sendable>(
  _ a: T, _ b: T
) -> T where T == T.TangentVector {
  let result1 = syncWorkload1Generic(a, b)
  let result2 = syncWorkload2Generic(a, b)
  let result3 = syncWorkload2Generic(b, a)
  return result1 + result2 * result3
}

// cfg.swift: common children precede the branch; result3 is local.

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

// loop.swift / loop-double.swift: retain the inclusive range 0...n.

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

// ternary.swift: retain the inclusive range 0...n.

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

// work-sleep.swift: preserve blocking forward work, not its 1 s delay.

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

// Extra arity regression: unlike the original ternary parent, this
// actually captures three distinct values. The c contribution must be zero.

@differentiable(reverse)
public func threeCapturesGeneric<T: Differentiable & FloatingPoint & Sendable>(
  _ a: T, _ b: T, _ c: T
) async -> T where T == T.TangentVector {
  async let y = workload3Generic(a, b, c)
  return await y
}

// Extra result-ownership regression: the task returns its capture as is.

@differentiable(reverse)
public func identityChildGeneric<T: Differentiable & Sendable>(
  _ x: T
) async -> T {
  async let y = x
  return await y
}
