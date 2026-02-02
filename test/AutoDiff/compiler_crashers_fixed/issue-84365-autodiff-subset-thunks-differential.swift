// RUN: %target-swift-frontend -emit-sil -verify %s

// https://github.com/swiftlang/swift/issues/84365
// Ensure autodiff subset thunks for differential correctly
// handle multiple semantic results and release unwanted
// result values

import _Differentiation

@differentiable(reverse,wrt: logits)
public func softSolveForwardWithQ(logits: [Float]) -> ([Float], [Float]) {
    return ([Float](repeating: 0, count: 0), [])
}

@derivative(of: softSolveForwardWithQ, wrt: logits)
public func vjpSoftSolveForwardWithQ(logits: [Float]) -> (value: ([Float], [Float]), pullback: ([Float].TangentVector, [Float].TangentVector) -> [Float].TangentVector) {
    let n = logits.count
    let q = [Float](repeating: 0, count: 0)
    let y = [Float](repeating: 0, count: 0)

    return (
        value: (y, q),
        pullback: { _, _ in 
            return Array<Float>.DifferentiableView([Float](repeating: 0, count: n))
        }
    )
}

@differentiable(reverse,wrt: logits)
public func forwardPredict(logits: [Float]) -> ([Float], [Float], [Float]) {
    let (y, q) = softSolveForwardWithQ(logits: logits)
    return (y, q, [0.0])
}

@derivative(of: forwardPredict, wrt: logits)
public func vjpForwardPredict(logits: [Float]) -> (
    value: ([Float], [Float], [Float]),
    pullback: ([Float].TangentVector, [Float].TangentVector, [Float].TangentVector) -> [Float].TangentVector
) {
    let (valYQ, pb) = vjpSoftSolveForwardWithQ(logits: logits)
    let (y, q) = valYQ
    return ((y, q, [0.0]), { upY, upQ, _ in pb(upY, upQ) }) 
}

@differentiable(reverse,wrt: logits)
public func crossEntropyFromForwardPredict(logits: [Float]) -> Float {
    let (_, q, _) = forwardPredict(logits: logits)
    return q[0] + 1e-8
}
