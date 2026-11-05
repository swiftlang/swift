import _Differentiation

@differentiable(reverse)
public func plainHeap(_ x: HeapPair) async -> HeapPair {
  return x
}

@differentiable(reverse)
public func squareHeap(_ x: HeapPair) async -> HeapPair {
  async let y = x * x
  return await y
}

@differentiable(reverse)
public func notUsefulHeap(_ x: HeapPair) async -> HeapPair {
  async let y = x
  let z = await y
  _ = z
  return .zero
}

@differentiable(reverse, wrt: x)
public func notVariedHeap(_ x: HeapPair, _ z: HeapPair) async -> HeapPair {
  async let y = z
  return await y
}

@differentiable(reverse)
public func workload1Heap(_ a: HeapPair, _ b: HeapPair) async -> HeapPair {
  return a + b
}

@differentiable(reverse)
public func workload2Heap(_ a: HeapPair, _ b: HeapPair) async -> HeapPair {
  return a * b
}

@differentiable(reverse)
public func workload3Heap(_ a: HeapPair, _ b: HeapPair, _ c: HeapPair) async -> HeapPair {
  return a * b
}

@differentiable(reverse)
public func doWorkHeap(_ a: HeapPair, _ b: HeapPair) async -> HeapPair {
  async let result1 = workload1Heap(a, b)
  async let result2 = workload2Heap(a, b)
  async let result3 = workload2Heap(b, a)
  return await result1 + result2 * result3
}

@differentiable(reverse, wrt: (a, b))
public func cfgHeap(_ a: HeapPair, _ b: HeapPair, _ flag: Bool) async -> HeapPair {
  async let result1 = workload1Heap(a, b)
  async let result2 = workload2Heap(a, b)
  if flag {
    async let result3 = workload2Heap(b, a)
    return await result1 + result2 * result3
  } else {
    async let result3 = workload1Heap(b, a)
    return await result1 * result2 + result3
  }
}

@differentiable(reverse, wrt: (a, b))
public func loopHeap(_ a: HeapPair, _ b: HeapPair, _ flag: Bool, _ n: Int) async -> HeapPair {
  var sum: HeapPair = .zero
  for i in 0...n {
    async let result0 = workload1Heap(HeapPair(Double(i), Double(i)) * a, b)
    sum += await result0
  }
  async let result1 = workload1Heap(a, b)
  async let result2 = workload2Heap(a, b)
  if flag {
    async let result3 = workload2Heap(b, a)
    return await sum * result1 + result2 * result3
  } else {
    async let result3 = workload1Heap(b, a)
    return await sum + result1 * result2 + result3
  }
}

@differentiable(reverse, wrt: (a, b))
public func ternaryHeap(_ a: HeapPair, _ b: HeapPair, _ flag: Bool, _ n: Int) async -> HeapPair {
  var sum: HeapPair = .zero
  for i in 0...n {
    async let result0 = workload1Heap(HeapPair(Double(i), Double(i)) * a, b)
    sum += await result0
  }
  async let result1 = workload1Heap(a, b)
  async let result2 = workload3Heap(a, b, a)
  if flag {
    async let result3 = workload3Heap(b, a, b)
    return await sum * result1 + result2 * result3
  } else {
    async let result3 = workload1Heap(b, a)
    return await sum + result1 * result2 + result3
  }
}

#if canImport(Glibc) || canImport(Darwin) || canImport(Musl) || canImport(Android)

@differentiable(reverse)
public func sleepWorkload1Heap(_ a: HeapPair, _ b: HeapPair) async -> HeapPair {
  asyncVJPBlockingSleep(milliseconds: 1)
  return a + b
}

@differentiable(reverse)
public func sleepWorkload2Heap(_ a: HeapPair, _ b: HeapPair) async -> HeapPair {
  asyncVJPBlockingSleep(milliseconds: 1)
  return a * b
}

@differentiable(reverse)
public func sleepWorkHeap(_ a: HeapPair, _ b: HeapPair) async -> HeapPair {
  async let result1 = sleepWorkload1Heap(a, b)
  async let result2 = sleepWorkload2Heap(a, b)
  return await result1 + result2
}

#endif

@differentiable(reverse)
public func threeCapturesHeap(_ a: HeapPair, _ b: HeapPair, _ c: HeapPair) async -> HeapPair {
  async let y = workload3Heap(a, b, c)
  return await y
}

@differentiable(reverse)
public func identityChildHeap(_ x: HeapPair) async -> HeapPair {
  async let y = x
  return await y
}
