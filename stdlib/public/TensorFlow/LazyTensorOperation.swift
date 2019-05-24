public class LazyTensor : _AnyTensorHandle {
  enum Handle {
    // Bool indicates if this was a result of materialization.
    case conc(TFETensorHandle, Bool)
    // Bool indiciates whether this is a live tensor.
    case sym(LazyTensorOperation, Int, Bool)
  }

  let handle: Handle

  public var _tfeTensorHandle: TFETensorHandle {
    switch (handle) {
      case Handle.conc(let h, _):
        return h
      case Handle.sym(let op, let index, _):
        return op.materialized(index: index)
    }
  }

  init(_ base: TFETensorHandle) {
    handle = Handle.conc(base, false)
  }

  init(_materialized base: TFETensorHandle) {
    handle = Handle.conc(base, true)
  }

  init(_lazy op: LazyTensorOperation, index: Int) {
    handle = Handle.sym(op, index, false)
    LazyTensor.incrementLiveCount(op)
  }

  init(_lazyLive op: LazyTensorOperation, index: Int) {
    handle = Handle.sym(op, index, true)
    LazyTensor.incrementLiveCount(op)
  }

  deinit {
    if case let Handle.sym(op, _, _) = handle {
      LazyTensor.decrementLiveCount(op)
    }
  }

  // Liveness tracking for LazyTensorOperations
  //

  private static var liveOperations: [
    ObjectIdentifier: (LazyTensorOperation,Int)] = [:]

  static func incrementLiveCount(_ op: LazyTensorOperation) {
    let opId = ObjectIdentifier(op)
    if let (lazyOp, count) = liveOperations[opId] {
      liveOperations[opId] = (lazyOp, count + 1)
    } else {
      liveOperations[opId] = (op, 1)
    }
  }

  static func decrementLiveCount(_ op: LazyTensorOperation) {
    let opId = ObjectIdentifier(op)
    if let (lazyOp, count) = liveOperations[opId] {
      if count > 1 {
        liveOperations[opId] = (lazyOp, count - 1)
      } else {
        liveOperations.removeValue(forKey: opId)
      }
    }
  }

  static func isLive(_ op: LazyTensorOperation) -> Bool {
    let opId = ObjectIdentifier(op)
    if let (_, count) = liveOperations[opId] {
      return count > 0
    }
    return false
  }

  static func onLiveOperations(_ perform: (LazyTensorOperation) -> ()) {
    for (_, (op,_)) in liveOperations { perform(op) }
  }

  public static func printLiveCount() {
    print("Live tensors: \(liveOperations.count)")
  }
}

public class LazyTensorOperation : TensorOperation {
  public typealias TensorValueHandle = LazyTensor

  enum Input {
    case single(LazyTensor)
    case list([LazyTensor])
  }

  enum Attribute {
    case BoolValue(Bool)
    case IntValue(Int)
    case FloatValue(Float)
    case DoubleValue(Double)
    case StringValue(String)
    case BoolArray([Bool])
    case IntArray([Int])
    case FloatArray([Float])
    case DoubleArray([Double])
    case StringArray([String])
    case TensorDataTypeValue(TensorDataType)
  }

  let name: String
  let outputCount: Int
  var inputs: [Input]
  var attrs: [String: Attribute]
  var outputs: [TFETensorHandle]?

  static var materializationCallback: (String) -> () = {
    (s: String) in return }

  public static func registerMaterializationCallback(
    f: @escaping (String) -> ()) {
    materializationCallback = f
  }

  public static var liveOperations: Int = 0

  public required init(_ name: String, _ outputCount: Int) {
    self.name = name
    self.inputs = []
    self.attrs = [:]
    self.outputCount = outputCount
    self.outputs = nil
    LazyTensorOperation.liveOperations += 1
  }

  deinit {
    LazyTensorOperation.liveOperations -= 1
  }

  public func evaluate() -> [LazyTensor] {
    return Array((0..<outputCount).map {
        LazyTensor(_lazyLive: self, index: $0)
      })
  }

  public func addInput(_ input : LazyTensor) {
    inputs.append(Input.single(input))
  }

  public func updateAttribute(_ name: String, _ value: Bool) {
    attrs[name] = Attribute.BoolValue(value)
  }
  public func updateAttribute(_ name: String, _ value: Int) {
    attrs[name] = Attribute.IntValue(value)
  }
  public func updateAttribute(_ name: String, _ value: Int32) {
    attrs[name] = Attribute.IntValue(Int(value))
  }
  public func updateAttribute(_ name: String, _ value: Int64) {
    attrs[name] = Attribute.IntValue(Int(value))
  }
  public func updateAttribute(_ name: String, _ value: Float) {
    attrs[name] = Attribute.FloatValue(value)
  }
  public func updateAttribute(_ name: String, _ value: Double) {
    attrs[name] = Attribute.DoubleValue(value)
  }
  public func updateAttribute(_ name: String, _ value: String) {
    attrs[name] = Attribute.StringValue(value)
  }
  public func updateAttribute(_ name: String, _ value: [Bool]) {
    attrs[name] = Attribute.BoolArray(value)
  }
  public func updateAttribute(_ name: String, _ value: [Int]) {
    attrs[name] = Attribute.IntArray(value)
  }
  public func updateAttribute(_ name: String, _ value: [Int32]) {
    attrs[name] = Attribute.IntArray(value.map { Int($0) })
  }
  public func updateAttribute(_ name: String, _ value: [Int64]) {
    attrs[name] = Attribute.IntArray(value.map { Int($0) })
  }
  public func updateAttribute(_ name: String, _ value: [Float]) {
    attrs[name] = Attribute.FloatArray(value)
  }
  public func updateAttribute(_ name: String, _ value: [Double]) {
    attrs[name] = Attribute.DoubleArray(value)
  }
  public func updateAttribute(_ name: String, _ value: [String]) {
    attrs[name] = Attribute.StringArray(value)
  }
}

extension LazyTensorOperation : TFTensorOperation {
  public func lazyTensorHandle(_ input: _AnyTensorHandle) -> LazyTensor {
    if let lazyHandle = input as? LazyTensor {
      if case let LazyTensor.Handle.sym(op, index, true) = lazyHandle.handle {
        return LazyTensor(_lazy: op, index: index)
      } else {
        return lazyHandle
      }
    } else {
      return LazyTensor(input._tfeTensorHandle)
    }
  }

  public func addInput(_ input: _AnyTensorHandle) {
    addInput(lazyTensorHandle(input))
  }

  public func addInput<Scalar: TensorFlowScalar>(_ input: Tensor<Scalar>) {
    addInput(input.handle.handle)
  }

  public func addInput(_ input: StringTensor) {
    addInput(input.handle.handle)
  }

  public func addInput(_ input: VariantHandle) {
    addInput(input.handle)
  }

  public func addInput(_ input: ResourceHandle) {
    addInput(input.handle)
  }

  public func addInputList<T: TensorArrayProtocol>(_ input: T) {
    let lazyHandles = input.tensorHandles().map { lazyTensorHandle($0) }
    inputs.append(Input.list(lazyHandles))
  }

  public func updateAttribute(_ name: String, _ value: TensorDataType) {
    attrs[name] = Attribute.TensorDataTypeValue(value)
  }
  public func updateAttribute(_ name: String, _ value: TensorShape) {
    assert(false, "Unimplemented TensorShape attribute.")
  }
  public func updateAttribute(_ name: String, _ value: TensorShape?) {
    assert(false, "Unimplemented TensorShape? attribute.")
  }
  public func updateAttribute(_ name: String, _ value: [TensorDataType]) {
    assert(false, "Unimplemented [TensorDataType] attribute.")
  }
  public func updateAttribute(_ name: String, _ value: [TensorShape]) {
    assert(false, "Unimplemented [TensorShape] attribute.")
  }
  public func updateAttribute(_ name: String, _ value: [TensorShape?]) {
    assert(false, "Unimplemented [TensorShape?] attribute.")
  }
  public func updateAttribute<In: TensorGroup, Out: TensorGroup>(
    _ name: String, _ value: (In) -> Out) {
    // TODO:
    assert(false, "Unimplemented [TFFunction] attribute.")
  }

  public func execute() {}

  public func execute<T0 : TensorArrayProtocol>(
    _ count0: Int
  ) -> (T0) {
    let outputs = evaluate()
    let offset0 = 0
    let result = (
      T0.init(handles: Array(outputs[offset0..<count0])))
    return result
  }

  public func execute<T0 : TensorArrayProtocol, T1 : TensorArrayProtocol>(
    _ count0: Int,
    _ count1: Int
  ) -> (T0, T1) {
    let outputs = evaluate()
    let offset0 = 0
    let offset1 = offset0 + count0
    let result = (
      T0.init(handles: Array(outputs[offset0..<offset1])),
      T1.init(handles: Array(outputs[offset1..<outputs.count])))
    return result
  }

  public func execute<T0 : TensorArrayProtocol, T1 : TensorArrayProtocol, T2 : TensorArrayProtocol>(
    _ count0: Int,
    _ count1: Int,
    _ count2: Int
  ) -> (T0, T1, T2) {
    let outputs = evaluate()
    let offset0 = 0
    let offset1 = offset0 + count0
    let offset2 = offset1 + count1
    let result = (
      T0.init(handles: Array(outputs[offset0..<offset1])),
      T1.init(handles: Array(outputs[offset1..<offset2])),
      T2.init(handles: Array(outputs[offset2..<outputs.count])))
    return result
  }

  public func execute<T0 : TensorArrayProtocol, T1 : TensorArrayProtocol, T2 : TensorArrayProtocol, T3 : TensorArrayProtocol>(
    _ count0: Int,
    _ count1: Int,
    _ count2: Int,
    _ count3: Int
  ) -> (T0, T1, T2, T3) {
    let outputs = evaluate()
    let offset0 = 0
    let offset1 = offset0 + count0
    let offset2 = offset1 + count1
    let offset3 = offset2 + count2
    let result = (
      T0.init(handles: Array(outputs[offset0..<offset1])),
      T1.init(handles: Array(outputs[offset1..<offset2])),
      T2.init(handles: Array(outputs[offset2..<offset3])),
      T3.init(handles: Array(outputs[offset3..<outputs.count])))
    return result
  }

  public func execute<T0 : TensorArrayProtocol, T1 : TensorArrayProtocol, T2 : TensorArrayProtocol, T3 : TensorArrayProtocol, T4 : TensorArrayProtocol>(
    _ count0: Int,
    _ count1: Int,
    _ count2: Int,
    _ count3: Int,
    _ count4: Int
  ) -> (T0, T1, T2, T3, T4) {
    let outputs = evaluate()
    let offset0 = 0
    let offset1 = offset0 + count0
    let offset2 = offset1 + count1
    let offset3 = offset2 + count2
    let offset4 = offset3 + count3
    let result = (
      T0.init(handles: Array(outputs[offset0..<offset1])),
      T1.init(handles: Array(outputs[offset1..<offset2])),
      T2.init(handles: Array(outputs[offset2..<offset3])),
      T3.init(handles: Array(outputs[offset3..<offset4])),
      T4.init(handles: Array(outputs[offset4..<outputs.count])))
    return result
  }

  public func execute<T0 : TensorArrayProtocol, T1 : TensorArrayProtocol, T2 : TensorArrayProtocol, T3 : TensorArrayProtocol, T4 : TensorArrayProtocol, T5 : TensorArrayProtocol>(
    _ count0: Int,
    _ count1: Int,
    _ count2: Int,
    _ count3: Int,
    _ count4: Int,
    _ count5: Int
  ) -> (T0, T1, T2, T3, T4, T5) {
    let outputs = evaluate()
    let offset0 = 0
    let offset1 = offset0 + count0
    let offset2 = offset1 + count1
    let offset3 = offset2 + count2
    let offset4 = offset3 + count3
    let offset5 = offset4 + count4
    let result = (
      T0.init(handles: Array(outputs[offset0..<offset1])),
      T1.init(handles: Array(outputs[offset1..<offset2])),
      T2.init(handles: Array(outputs[offset2..<offset3])),
      T3.init(handles: Array(outputs[offset3..<offset4])),
      T4.init(handles: Array(outputs[offset4..<offset5])),
      T5.init(handles: Array(outputs[offset5..<outputs.count])))
    return result
  }

  public func execute<T0 : TensorArrayProtocol, T1 : TensorArrayProtocol, T2 : TensorArrayProtocol, T3 : TensorArrayProtocol, T4 : TensorArrayProtocol, T5 : TensorArrayProtocol, T6 : TensorArrayProtocol>(
    _ count0: Int,
    _ count1: Int,
    _ count2: Int,
    _ count3: Int,
    _ count4: Int,
    _ count5: Int,
    _ count6: Int
  ) -> (T0, T1, T2, T3, T4, T5, T6) {
    let outputs = evaluate()
    let offset0 = 0
    let offset1 = offset0 + count0
    let offset2 = offset1 + count1
    let offset3 = offset2 + count2
    let offset4 = offset3 + count3
    let offset5 = offset4 + count4
    let offset6 = offset5 + count5
    let result = (
      T0.init(handles: Array(outputs[offset0..<offset1])),
      T1.init(handles: Array(outputs[offset1..<offset2])),
      T2.init(handles: Array(outputs[offset2..<offset3])),
      T3.init(handles: Array(outputs[offset3..<offset4])),
      T4.init(handles: Array(outputs[offset4..<offset5])),
      T5.init(handles: Array(outputs[offset5..<offset6])),
      T6.init(handles: Array(outputs[offset6..<outputs.count])))
    return result
  }

  public func execute<T0 : TensorArrayProtocol, T1 : TensorArrayProtocol, T2 : TensorArrayProtocol, T3 : TensorArrayProtocol, T4 : TensorArrayProtocol, T5 : TensorArrayProtocol, T6 : TensorArrayProtocol, T7 : TensorArrayProtocol>(
    _ count0: Int,
    _ count1: Int,
    _ count2: Int,
    _ count3: Int,
    _ count4: Int,
    _ count5: Int,
    _ count6: Int,
    _ count7: Int
  ) -> (T0, T1, T2, T3, T4, T5, T6, T7) {
    let outputs = evaluate()
    let offset0 = 0
    let offset1 = offset0 + count0
    let offset2 = offset1 + count1
    let offset3 = offset2 + count2
    let offset4 = offset3 + count3
    let offset5 = offset4 + count4
    let offset6 = offset5 + count5
    let offset7 = offset6 + count6
    let result = (
      T0.init(handles: Array(outputs[offset0..<offset1])),
      T1.init(handles: Array(outputs[offset1..<offset2])),
      T2.init(handles: Array(outputs[offset2..<offset3])),
      T3.init(handles: Array(outputs[offset3..<offset4])),
      T4.init(handles: Array(outputs[offset4..<offset5])),
      T5.init(handles: Array(outputs[offset5..<offset6])),
      T6.init(handles: Array(outputs[offset6..<offset7])),
      T7.init(handles: Array(outputs[offset7..<outputs.count])))
    return result
  }

  public func execute<T0 : TensorArrayProtocol, T1 : TensorArrayProtocol, T2 : TensorArrayProtocol, T3 : TensorArrayProtocol, T4 : TensorArrayProtocol, T5 : TensorArrayProtocol, T6 : TensorArrayProtocol, T7 : TensorArrayProtocol, T8 : TensorArrayProtocol>(
    _ count0: Int,
    _ count1: Int,
    _ count2: Int,
    _ count3: Int,
    _ count4: Int,
    _ count5: Int,
    _ count6: Int,
    _ count7: Int,
    _ count8: Int
  ) -> (T0, T1, T2, T3, T4, T5, T6, T7, T8) {
    let outputs = evaluate()
    let offset0 = 0
    let offset1 = offset0 + count0
    let offset2 = offset1 + count1
    let offset3 = offset2 + count2
    let offset4 = offset3 + count3
    let offset5 = offset4 + count4
    let offset6 = offset5 + count5
    let offset7 = offset6 + count6
    let offset8 = offset7 + count7
    let result = (
      T0.init(handles: Array(outputs[offset0..<offset1])),
      T1.init(handles: Array(outputs[offset1..<offset2])),
      T2.init(handles: Array(outputs[offset2..<offset3])),
      T3.init(handles: Array(outputs[offset3..<offset4])),
      T4.init(handles: Array(outputs[offset4..<offset5])),
      T5.init(handles: Array(outputs[offset5..<offset6])),
      T6.init(handles: Array(outputs[offset6..<offset7])),
      T7.init(handles: Array(outputs[offset7..<offset8])),
      T8.init(handles: Array(outputs[offset8..<outputs.count])))
    return result
  }

  public func execute<T0 : TensorArrayProtocol, T1 : TensorArrayProtocol, T2 : TensorArrayProtocol, T3 : TensorArrayProtocol, T4 : TensorArrayProtocol, T5 : TensorArrayProtocol, T6 : TensorArrayProtocol, T7 : TensorArrayProtocol, T8 : TensorArrayProtocol, T9 : TensorArrayProtocol>(
    _ count0: Int,
    _ count1: Int,
    _ count2: Int,
    _ count3: Int,
    _ count4: Int,
    _ count5: Int,
    _ count6: Int,
    _ count7: Int,
    _ count8: Int,
    _ count9: Int
  ) -> (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9) {
    let outputs = evaluate()
    let offset0 = 0
    let offset1 = offset0 + count0
    let offset2 = offset1 + count1
    let offset3 = offset2 + count2
    let offset4 = offset3 + count3
    let offset5 = offset4 + count4
    let offset6 = offset5 + count5
    let offset7 = offset6 + count6
    let offset8 = offset7 + count7
    let offset9 = offset8 + count8
    let result = (
      T0.init(handles: Array(outputs[offset0..<offset1])),
      T1.init(handles: Array(outputs[offset1..<offset2])),
      T2.init(handles: Array(outputs[offset2..<offset3])),
      T3.init(handles: Array(outputs[offset3..<offset4])),
      T4.init(handles: Array(outputs[offset4..<offset5])),
      T5.init(handles: Array(outputs[offset5..<offset6])),
      T6.init(handles: Array(outputs[offset6..<offset7])),
      T7.init(handles: Array(outputs[offset7..<offset8])),
      T8.init(handles: Array(outputs[offset8..<offset9])),
      T9.init(handles: Array(outputs[offset9..<outputs.count])))
    return result
  }
}

extension LazyTensorOperation : CustomStringConvertible {
  func lazyTensorDescription(_ lazyTensor: LazyTensor) -> String {
    switch lazyTensor.handle {
      case LazyTensor.Handle.conc(_):
        return "conc"
      case LazyTensor.Handle.sym(_, let index, let isLive):
        return "(sym, \(index), \(isLive))"
    }
  }

  public var description: String {
    let attrsDesc = attrs.map { (name, value) in "\(name): \(value)" }
    let inputsDesc = inputs.map { (input: Input) -> String in
      switch input {
        case Input.single(let lazyTensor):
          return lazyTensorDescription(lazyTensor)
        case Input.list(let lazyTensorList): do {
          let lazyTensors = lazyTensorList.map{ lazyTensorDescription($0) }
          let lazyTensorsDesc = lazyTensors.joined(separator: ", ")
          return "[\(lazyTensorsDesc)]"
        }
      }
    }
    var desc = "\(name)["
    desc += attrsDesc.joined(separator: ", ")
    desc += "]("
    desc += inputsDesc.joined(separator: ", ")
    desc += ")"
    return desc
  }
}
