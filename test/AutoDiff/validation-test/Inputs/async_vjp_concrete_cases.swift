import _Differentiation

// plain.swift: no task, an ordinary async VJP baseline.

@differentiable(reverse)
public func plainFloat(_ x: Float) async -> Float {
  return x
}

// src.swift: one task; both occurrences of x contribute.

@differentiable(reverse)
public func squareFloat(_ x: Float) async -> Float {
  async let y = x * x
  return await y
}

// notUseful.swift: varied child result, unused by the return.

@differentiable(reverse)
public func notUsefulFloat(_ x: Float) async -> Float {
  async let y = x
  let z = await y
  _ = z
  return 0
}

// notVaried.swift: only x is selected; the child depends on z.

@differentiable(reverse, wrt: x)
public func notVariedFloat(_ x: Float, _ z: Float) async -> Float {
  async let y = z
  return await y
}

@differentiable(reverse)
public func workload1Float(_ a: Float, _ b: Float) async -> Float {
  return a + b
}

@differentiable(reverse)
public func workload2Float(_ a: Float, _ b: Float) async -> Float {
  return a * b
}

// ternary.swift deliberately does not use c.

@differentiable(reverse)
public func workload3Float(_ a: Float, _ b: Float, _ c: Float) async -> Float {
  return a * b
}

// dowork.swift / generic.swift: the third capture order is b, a.

@differentiable(reverse)
public func doWorkFloat(_ a: Float, _ b: Float) async -> Float {
  async let result1 = workload1Float(a, b)
  async let result2 = workload2Float(a, b)
  async let result3 = workload2Float(b, a)
  return await result1 + result2 * result3
}

// dowork-sync.swift: ordinary synchronous control, not the oracle.

@differentiable(reverse)
public func syncWorkload1Float(_ a: Float, _ b: Float) -> Float {
  return a + b
}

@differentiable(reverse)
public func syncWorkload2Float(_ a: Float, _ b: Float) -> Float {
  return a * b
}

@differentiable(reverse)
public func doWorkSyncFloat(_ a: Float, _ b: Float) -> Float {
  let result1 = syncWorkload1Float(a, b)
  let result2 = syncWorkload2Float(a, b)
  let result3 = syncWorkload2Float(b, a)
  return result1 + result2 * result3
}

// cfg.swift: common children precede the branch; result3 is local.

@differentiable(reverse, wrt: (a, b))
public func cfgFloat(_ a: Float, _ b: Float, _ flag: Bool) async -> Float {
  async let result1 = workload1Float(a, b)
  async let result2 = workload2Float(a, b)
  if flag {
    async let result3 = workload2Float(b, a)
    return await result1 + result2 * result3
  } else {
    async let result3 = workload1Float(b, a)
    return await result1 * result2 + result3
  }
}

// loop.swift / loop-double.swift: retain the inclusive range 0...n.

@differentiable(reverse, wrt: (a, b))
public func loopFloat(_ a: Float, _ b: Float, _ flag: Bool, _ n: Int) async -> Float {
  var sum: Float = 0
  for i in 0...n {
    async let result0 = workload1Float(Float(i) * a, b)
    sum += await result0
  }
  async let result1 = workload1Float(a, b)
  async let result2 = workload2Float(a, b)
  if flag {
    async let result3 = workload2Float(b, a)
    return await sum * result1 + result2 * result3
  } else {
    async let result3 = workload1Float(b, a)
    return await sum + result1 * result2 + result3
  }
}

// ternary.swift: retain the inclusive range 0...n.

@differentiable(reverse, wrt: (a, b))
public func ternaryFloat(_ a: Float, _ b: Float, _ flag: Bool, _ n: Int) async -> Float {
  var sum: Float = 0
  for i in 0...n {
    async let result0 = workload1Float(Float(i) * a, b)
    sum += await result0
  }
  async let result1 = workload1Float(a, b)
  async let result2 = workload3Float(a, b, a)
  if flag {
    async let result3 = workload3Float(b, a, b)
    return await sum * result1 + result2 * result3
  } else {
    async let result3 = workload1Float(b, a)
    return await sum + result1 * result2 + result3
  }
}

#if canImport(Glibc) || canImport(Darwin) || canImport(Musl) || canImport(Android)

// work-sleep.swift: preserve blocking forward work, not its 1 s delay.

@differentiable(reverse)
public func sleepWorkload1Float(_ a: Float, _ b: Float) async -> Float {
  asyncVJPBlockingSleep(milliseconds: 1)
  return a + b
}

@differentiable(reverse)
public func sleepWorkload2Float(_ a: Float, _ b: Float) async -> Float {
  asyncVJPBlockingSleep(milliseconds: 1)
  return a * b
}

@differentiable(reverse)
public func sleepWorkFloat(_ a: Float, _ b: Float) async -> Float {
  async let result1 = sleepWorkload1Float(a, b)
  async let result2 = sleepWorkload2Float(a, b)
  return await result1 + result2
}

#endif

// Extra arity regression: unlike the original ternary parent, this
// actually captures three distinct values. The c contribution must be zero.

@differentiable(reverse)
public func threeCapturesFloat(_ a: Float, _ b: Float, _ c: Float) async -> Float {
  async let y = workload3Float(a, b, c)
  return await y
}

// Extra result-ownership regression: the task returns its capture as is.

@differentiable(reverse)
public func identityChildFloat(_ x: Float) async -> Float {
  async let y = x
  return await y
}

// plain.swift: no task, an ordinary async VJP baseline.

@differentiable(reverse)
public func plainDouble(_ x: Double) async -> Double {
  return x
}

// src.swift: one task; both occurrences of x contribute.

@differentiable(reverse)
public func squareDouble(_ x: Double) async -> Double {
  async let y = x * x
  return await y
}

// notUseful.swift: varied child result, unused by the return.

@differentiable(reverse)
public func notUsefulDouble(_ x: Double) async -> Double {
  async let y = x
  let z = await y
  _ = z
  return 0
}

// notVaried.swift: only x is selected; the child depends on z.

@differentiable(reverse, wrt: x)
public func notVariedDouble(_ x: Double, _ z: Double) async -> Double {
  async let y = z
  return await y
}

@differentiable(reverse)
public func workload1Double(_ a: Double, _ b: Double) async -> Double {
  return a + b
}

@differentiable(reverse)
public func workload2Double(_ a: Double, _ b: Double) async -> Double {
  return a * b
}

// ternary.swift deliberately does not use c.

@differentiable(reverse)
public func workload3Double(_ a: Double, _ b: Double, _ c: Double) async -> Double {
  return a * b
}

// dowork.swift / generic.swift: the third capture order is b, a.

@differentiable(reverse)
public func doWorkDouble(_ a: Double, _ b: Double) async -> Double {
  async let result1 = workload1Double(a, b)
  async let result2 = workload2Double(a, b)
  async let result3 = workload2Double(b, a)
  return await result1 + result2 * result3
}

// dowork-sync.swift: ordinary synchronous control, not the oracle.

@differentiable(reverse)
public func syncWorkload1Double(_ a: Double, _ b: Double) -> Double {
  return a + b
}

@differentiable(reverse)
public func syncWorkload2Double(_ a: Double, _ b: Double) -> Double {
  return a * b
}

@differentiable(reverse)
public func doWorkSyncDouble(_ a: Double, _ b: Double) -> Double {
  let result1 = syncWorkload1Double(a, b)
  let result2 = syncWorkload2Double(a, b)
  let result3 = syncWorkload2Double(b, a)
  return result1 + result2 * result3
}

// cfg.swift: common children precede the branch; result3 is local.

@differentiable(reverse, wrt: (a, b))
public func cfgDouble(_ a: Double, _ b: Double, _ flag: Bool) async -> Double {
  async let result1 = workload1Double(a, b)
  async let result2 = workload2Double(a, b)
  if flag {
    async let result3 = workload2Double(b, a)
    return await result1 + result2 * result3
  } else {
    async let result3 = workload1Double(b, a)
    return await result1 * result2 + result3
  }
}

// loop.swift / loop-double.swift: retain the inclusive range 0...n.

@differentiable(reverse, wrt: (a, b))
public func loopDouble(_ a: Double, _ b: Double, _ flag: Bool, _ n: Int) async -> Double {
  var sum: Double = 0
  for i in 0...n {
    async let result0 = workload1Double(Double(i) * a, b)
    sum += await result0
  }
  async let result1 = workload1Double(a, b)
  async let result2 = workload2Double(a, b)
  if flag {
    async let result3 = workload2Double(b, a)
    return await sum * result1 + result2 * result3
  } else {
    async let result3 = workload1Double(b, a)
    return await sum + result1 * result2 + result3
  }
}

// ternary.swift: retain the inclusive range 0...n.

@differentiable(reverse, wrt: (a, b))
public func ternaryDouble(_ a: Double, _ b: Double, _ flag: Bool, _ n: Int) async -> Double {
  var sum: Double = 0
  for i in 0...n {
    async let result0 = workload1Double(Double(i) * a, b)
    sum += await result0
  }
  async let result1 = workload1Double(a, b)
  async let result2 = workload3Double(a, b, a)
  if flag {
    async let result3 = workload3Double(b, a, b)
    return await sum * result1 + result2 * result3
  } else {
    async let result3 = workload1Double(b, a)
    return await sum + result1 * result2 + result3
  }
}

#if canImport(Glibc) || canImport(Darwin) || canImport(Musl) || canImport(Android)

// work-sleep.swift: preserve blocking forward work, not its 1 s delay.

@differentiable(reverse)
public func sleepWorkload1Double(_ a: Double, _ b: Double) async -> Double {
  asyncVJPBlockingSleep(milliseconds: 1)
  return a + b
}

@differentiable(reverse)
public func sleepWorkload2Double(_ a: Double, _ b: Double) async -> Double {
  asyncVJPBlockingSleep(milliseconds: 1)
  return a * b
}

@differentiable(reverse)
public func sleepWorkDouble(_ a: Double, _ b: Double) async -> Double {
  async let result1 = sleepWorkload1Double(a, b)
  async let result2 = sleepWorkload2Double(a, b)
  return await result1 + result2
}

#endif

// Extra arity regression: unlike the original ternary parent, this
// actually captures three distinct values. The c contribution must be zero.

@differentiable(reverse)
public func threeCapturesDouble(_ a: Double, _ b: Double, _ c: Double) async -> Double {
  async let y = workload3Double(a, b, c)
  return await y
}

// Extra result-ownership regression: the task returns its capture as is.

@differentiable(reverse)
public func identityChildDouble(_ x: Double) async -> Double {
  async let y = x
  return await y
}
