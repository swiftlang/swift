import TensorFlow

public func conflictingFunction(_ n: Int) -> Tensor<Double> {
  var i = 0
  var t = Tensor(0)
  while i < n {
    t += 2
    i += 1
  }
  return t
}
