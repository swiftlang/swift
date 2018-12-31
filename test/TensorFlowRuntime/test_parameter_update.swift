import TensorFlow

struct T : VectorNumeric, Differentiable {
  typealias TangentVector = T
  typealias CotangentVector = T
  var x: Float
}

T(x: 0).moved(along: T(x: 0))

struct Model : Parameterized {
  // typealias TangentVector = Parameters.TangentVector
  // typealias CotangentVector = Parameters.CotangentVector
  @TFParameter var w: Tensor<Float>

  func moved(along direction: Parameters.TangentVector) -> Model {
    return Model(w: allParameters.moved(along: direction).w)
  }
}
