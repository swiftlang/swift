// RUN: %target-swift-frontend -emit-sil -module-name test -verify %s

// https://github.com/swiftlang/swift/issues/92623

// Differentiating a call to a function with a `@differentiable(reverse) (T) throws -> R`
// closure parameter and a registered @derivative produces a corrupted pullback whenever the
// differentiable parameter is an aggregate (e.g. [Double]).
//
// The custom VJP *is* called and its pullback *is* invoked with the right seed (see prints),
// but the AD-generated glue that consumes the result is miscompiled:
//   - gradient silently comes back as zero ([]), or
//   - the process dies with heap corruption in malloc (more allocation traffic -> more likely).
//
// The identical shape with a scalar `Double` differentiable parameter works, and the identical
// non-throwing shape (control below) works.
import _Differentiation

// ===== broken: throwing closure param, [Double] differentiable param =====
@differentiable(reverse, wrt: xs)
func mapArr(_ xs: [Double], _ body: @differentiable(reverse) (Double) throws -> Double) throws -> [Double] {
    try xs.map(body)
}

@derivative(of: mapArr, wrt: xs)
func vjpMapArr(_ xs: [Double], _ body: @differentiable(reverse) (Double) throws -> Double) throws -> (
    value: [Double], pullback: (Array<Double>.TangentVector) -> Array<Double>.TangentVector
) {
    var values: [Double] = []
    var pullbacks: [(Double) -> Double] = []
    for x in xs {
        let (y, pb) = try valueWithPullback(at: x, of: body)
        values.append(y)
        pullbacks.append(pb)
    }
    print("  [vjpMapArr called, \(values.count) elements]")
    return (values, { tans in
        print("  [custom pullback invoked with seed \(tans)]")
        return .init(zip(tans.base, pullbacks).map { t, pb in pb(t) })
    })
}

// ===== control: same shape, nothing throws — works =====
@differentiable(reverse, wrt: xs)
func mapArrNT(_ xs: [Double], _ body: @differentiable(reverse) (Double) -> Double) -> [Double] {
    xs.map(body)
}

@derivative(of: mapArrNT, wrt: xs)
func vjpMapArrNT(_ xs: [Double], _ body: @differentiable(reverse) (Double) -> Double) -> (
    value: [Double], pullback: (Array<Double>.TangentVector) -> Array<Double>.TangentVector
) {
    var values: [Double] = []
    var pullbacks: [(Double) -> Double] = []
    for x in xs {
        let (y, pb) = valueWithPullback(at: x, of: body)
        values.append(y)
        pullbacks.append(pb)
    }
    return (values, { tans in .init(zip(tans.base, pullbacks).map { t, pb in pb(t) }) })
}

@differentiable(reverse)
func g(_ x: [Double]) throws -> [Double] { try mapArr(x) { $0 * $0 * $0 } }

@differentiable(reverse)
func gNT(_ x: [Double]) -> [Double] { mapArrNT(x) { $0 * $0 * $0 } }

print("non-throwing control:")
let (v2, pb2) = valueWithPullback(at: [1.0, 2.0, 3.0], of: gNT)
print("value:", v2, "grad:", pb2(.init([1.0, 1.0, 1.0])), "(expect [3.0, 12.0, 27.0])")

print("throwing version:")
let (v, pb) = try valueWithPullback(at: [1.0, 2.0, 3.0], of: g)
print("value:", v, "grad:", pb(.init([1.0, 1.0, 1.0])), "(expect [3.0, 12.0, 27.0])")
