import TensorFlow

@_fixed_layout
public struct ExternalStructFixedLayout {
  let x: Tensor<Float>
}

extension ExternalStructFixedLayout : TensorGroup {
  public func _unpackTensorHandles(into address: UnsafeMutablePointer<CTensorHandle>?) {
    fatalError("dummy conformance")
  }
  public static var _typeList: [TensorDataType] {
    fatalError("dummy conformance")
  }
  public static var _unknownShapeList: [TensorShape?] {
    fatalError("dummy conformance")
  }
  public init(_owning tensorHandles: UnsafePointer<CTensorHandle>?) {
    fatalError("dummy conformance")
  }
}
