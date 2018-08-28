import TensorFlow

public struct ExternalStructNotFixedLayout {
  let x: Tensor<Float>
}

@_fixed_layout
public struct ExternalStructFixedLayout {
  let x: Tensor<Float>
}
