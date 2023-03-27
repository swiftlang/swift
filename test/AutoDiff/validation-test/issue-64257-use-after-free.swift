// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import _Differentiation
public func simWithPullback(params: MinParams) -> (value: Output, pullback: (Output.TangentVector) -> (MinParams.TangentVector)){
    let simulationValueAndPullback = valueWithPullback(at: params, of: run)
    return (value: simulationValueAndPullback.value, pullback: simulationValueAndPullback.pullback)
}

@differentiable(reverse)
public func run(params: MinParams) -> Output {
    for t in 0 ... 1 {
    }

    let res = MiniLoop(other: params._twoDArray).results
    return Output(results: res)
}

struct MiniLoop: Differentiable {
    var results: Float
    var twoDArray: Float
  
    @differentiable(reverse)
    init(results: Float = 146, other: Float = 123) {self.results = results; self.twoDArray = other}
}

public struct Output: Differentiable {
    public var results: Float
    @differentiable(reverse)
    public init(results: Float) {self.results = results}
}

public struct MinParams: Differentiable {
    public var _twoDArray: Float
    public init(foo: Float = 42) { _twoDArray = foo }
}

func test() {
    let valueAndPullback = simWithPullback(params: MinParams())
    let output = valueAndPullback.value
    let resultOnes = Float(1.0)
    var grad = valueAndPullback.pullback(Output.TangentVector(results: resultOnes))
    print(grad)
    grad = valueAndPullback.pullback(Output.TangentVector(results: resultOnes))
    print(grad)
    grad = valueAndPullback.pullback(Output.TangentVector(results: resultOnes))
    print(grad)
}

test()
