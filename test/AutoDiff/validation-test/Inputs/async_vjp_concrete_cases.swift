import _Differentiation

@differentiable(reverse)
public func plainFloat(_ x: Float) async -> Float {
  return x
}

@differentiable(reverse)
public func squareFloat(_ x: Float) async -> Float {
  async let y = x * x
  return await y
}

@differentiable(reverse)
public func notUsefulFloat(_ x: Float) async -> Float {
  async let y = x
  let z = await y
  _ = z
  return 0
}

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

@differentiable(reverse)
public func workload3Float(_ a: Float, _ b: Float, _ c: Float) async -> Float {
  return a * b
}

@differentiable(reverse)
public func doWorkFloat(_ a: Float, _ b: Float) async -> Float {
  async let result1 = workload1Float(a, b)
  async let result2 = workload2Float(a, b)
  async let result3 = workload2Float(b, a)
  return await result1 + result2 * result3
}

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

@differentiable(reverse)
public func threeCapturesFloat(_ a: Float, _ b: Float, _ c: Float) async -> Float {
  async let y = workload3Float(a, b, c)
  return await y
}

@differentiable(reverse)
public func identityChildFloat(_ x: Float) async -> Float {
  async let y = x
  return await y
}

@differentiable(reverse, wrt: x)
public func extractedVJPChildFloat(
  _ x: Float, _ body: @differentiable(reverse) (Float, Float) -> Float
) async -> Float {
  async let y = body(2, x)
  return await y
}

@differentiable(reverse)
public func plainDouble(_ x: Double) async -> Double {
  return x
}

@differentiable(reverse)
public func squareDouble(_ x: Double) async -> Double {
  async let y = x * x
  return await y
}

@differentiable(reverse)
public func notUsefulDouble(_ x: Double) async -> Double {
  async let y = x
  let z = await y
  _ = z
  return 0
}

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

@differentiable(reverse)
public func workload3Double(_ a: Double, _ b: Double, _ c: Double) async -> Double {
  return a * b
}

@differentiable(reverse)
public func doWorkDouble(_ a: Double, _ b: Double) async -> Double {
  async let result1 = workload1Double(a, b)
  async let result2 = workload2Double(a, b)
  async let result3 = workload2Double(b, a)
  return await result1 + result2 * result3
}

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

@differentiable(reverse)
public func threeCapturesDouble(_ a: Double, _ b: Double, _ c: Double) async -> Double {
  async let y = workload3Double(a, b, c)
  return await y
}

@differentiable(reverse)
public func identityChildDouble(_ x: Double) async -> Double {
  async let y = x
  return await y
}
