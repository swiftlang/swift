// RUN: %{python} %S/../Inputs/timeout.py 20 %target-swift-frontend -O %s -emit-sil

func genericChain<T, U, V>(
  input: T, depth: Int, toU: @escaping (T) -> U,
  toV: @escaping (U) -> V, toT: @escaping (V) -> T
) -> T {
  if depth
    <= 0
  {
    return toT(toV(toU(input)))
  }
  return genericChain(
    input: toT(toV(toU(input))), depth: depth - 1,
    toU: { t in toU(toT(toV(toU(t)))) },
    toV: { u in toV(toU(toT(toV(u)))) },
    toT: { v in toT(toV(toU(toT(v)))) })
}

let result4 = genericChain(
  input: 10,
  depth: 0,
  toU: { Double($0) },
  toV: { String(Int($0) % 10000) },
  toT: { Int($0) ?? 0 }
)
print("genericChain result: \(result4)")
