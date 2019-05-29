import CTensorFlow

/// The `TF_Tensor *` type.
typealias CTFTensor = OpaquePointer

public class LazyTensor : _AnyTensorHandle {
  enum Handle {
    // Bool indicates if this was a result of materialization.
    case conc(TFETensorHandle, Bool)
    // Bool indicates whether this is a live tensor. i.e.,
    // is this held by a swift variable.
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
    LazyTensor.incrementRefCount(op, isLive: false)
  }

  init(_lazyLive op: LazyTensorOperation, index: Int) {
    handle = Handle.sym(op, index, true)
    LazyTensor.incrementRefCount(op, isLive: true)
  }

  deinit {
    if case let Handle.sym(op, _, isLive) = handle {
      LazyTensor.decrementRefCount(op, isLive: isLive)
    }
  }

  // Liveness tracking for LazyTensorOperations
  //
  struct LazyTensorOperationRefCounts {
    let op: LazyTensorOperation
    let live: Int
    let all: Int
  }

  private static var operationRefCounts: [
    ObjectIdentifier: LazyTensorOperationRefCounts] = [:]

  static func incrementRefCount(_ op: LazyTensorOperation, isLive: Bool) {
    let opId = ObjectIdentifier(op)
    if let counts = operationRefCounts[opId] {
      operationRefCounts[opId] = LazyTensorOperationRefCounts(
        op: op,
        live: isLive ? counts.live + 1 : counts.live,
        all: counts.all + 1)
    } else {
      operationRefCounts[opId] = LazyTensorOperationRefCounts(
        op: op, live: isLive ? 1 : 0, all: 1)
    }
  }

  static func decrementRefCount(_ op: LazyTensorOperation, isLive: Bool) {
    let opId = ObjectIdentifier(op)
    if let counts = operationRefCounts[opId] {
      if counts.all > 1 {
        operationRefCounts[opId] = LazyTensorOperationRefCounts(
          op: op,
          live: isLive ? counts.live - 1 : counts.live,
          all: counts.all - 1)
      } else {
        operationRefCounts.removeValue(forKey: opId)
      }
    }
  }

  static func isLive(_ op: LazyTensorOperation) -> Bool {
    let opId = ObjectIdentifier(op)
    if let counts = operationRefCounts[opId] {
      return counts.live > 0
    }
    return false
  }

  static func onLiveOperations(_ perform: (LazyTensorOperation) -> ()) {
    for (_, counts) in operationRefCounts {
      if (counts.live > 0) { perform(counts.op) }
    }
  }

  static func onAllOperations(_ perform: (LazyTensorOperation) -> ()) {
    for (_, counts) in operationRefCounts { perform(counts.op) }
  }

  public static func printRefCounts() {
    let live = operationRefCounts.values.reduce(0, { (sum, element) in
        return sum + (element.live > 0 ? 1 : 0)
      })
    print("LazyTensorOperations: \(operationRefCounts.count) (\(live) live)")
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
    case ConstTensor(TFETensorHandle)
    case TensorDataTypeValue(TensorDataType)
  }

  let name: String
  let outputCount: Int
  var inputs: [Input]
  var attrs: [String: Attribute]
  var outputs: [TFETensorHandle]?
  var alias: String?

  static public var materializationCallback: (String) -> () = {
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
    self.alias = nil
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


extension LazyTensorOperation {
  static public func makeSymbolic(_ input: _AnyTensorHandle) -> LazyTensor {
    return LazyTensor(_materialized: input._tfeTensorHandle)
  }

  static public func makeSymbolic<Scalar: TensorFlowScalar>(_ input: Tensor<Scalar>) -> Tensor<Scalar> {
    return Tensor(
      handle: TensorHandle<Scalar>(handle: makeSymbolic(input.handle.handle)))
  }

  static public func makeSymbolic(_ input: StringTensor) -> StringTensor {
    return StringTensor(
      handle: TensorHandle<String>(handle: makeSymbolic(input.handle.handle)))
  }

  static public func makeSymbolic(_ input: VariantHandle) -> VariantHandle {
    return VariantHandle(handle: makeSymbolic(input.handle))
  }

  static public func makeSymbolic(_ input: ResourceHandle) -> ResourceHandle {
    return ResourceHandle(handle: makeSymbolic(input.handle))
  }
}

extension LazyTensorOperation.Attribute : CustomStringConvertible {
  public var description: String {
    switch self {
    case .BoolValue(let v): return "\(v)"
    case .IntValue(let v): return "Int(\(v))"
    case .FloatValue(let v): return "Float(\(v))"
    case .DoubleValue(let v): return "Double(\(v))"
    case .StringValue(let v): return v
    case .BoolArray(let values): return arrayAsString("", values)
    case .IntArray(let values): return arrayAsString("Int", values)
    case .FloatArray(let values): return arrayAsString("Float", values)
    case .DoubleArray(let values): return arrayAsString("Double", values)
    case .StringArray(let values): return arrayAsString("", values)
    case .ConstTensor(let v): return "Const(\(v))"
    case .TensorDataTypeValue(let v): return dataTypeAsString(v)
    }
  }

  private func arrayAsString<T>(_ desc: String, _ values: [T]) -> String {
    let arrayDesc = (values.map { "\($0)" }).joined(separator: ",")
    return "\(desc)[\(arrayDesc)]"
  }

  private func dataTypeAsString(_ dataType: TensorDataType) -> String {
    switch dataType._cDataType {
    case TF_FLOAT: return "float"
    case TF_DOUBLE: return "double"
    case TF_INT32: return "int32"
    case TF_UINT8: return "uint8"
    case TF_INT16: return "int16"
    case TF_INT8: return "int8"
    case TF_STRING: return "string"
    case TF_COMPLEX64, TF_COMPLEX: return "complex"
    case TF_INT64: return "int64"
    case TF_BOOL: return "bool"
    case TF_QINT8: return "qint8"
    case TF_QUINT8: return "quint8"
    case TF_QINT32: return "qint32"
    case TF_BFLOAT16: return "bfloat16"
    case TF_QINT16: return "qint16"
    case TF_QUINT16: return "quint16"
    case TF_UINT16: return "uint16"
    case TF_COMPLEX128: return "complex128"
    case TF_HALF: return "half"
    case TF_RESOURCE: return "resource"
    case TF_VARIANT: return "variant"
    case TF_UINT32: return "uint32"
    case TF_UINT64: return "uint64"
    default: assert(false, "Unhandled type: \(dataType._cDataType)")
    }
  }
}

extension LazyTensorOperation : CustomStringConvertible {
  func lazyTensorDescription(_ lazyTensor: LazyTensor) -> String {
    switch lazyTensor.handle {
    case LazyTensor.Handle.conc(_):
      return "conc"
    case LazyTensor.Handle.sym(let op, let index, let isLive):
      return "(\(op.nameDescription()), \(index), \(isLive))"
    }
  }

  func nameDescription() -> String {
    if let alias = self.alias {
      return "\(alias)"
    } else {
      return "\(name)_\(ObjectIdentifier(self))"
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
    var desc = "\(nameDescription())["
    desc += attrsDesc.joined(separator: ", ")
    desc += "]("
    desc += inputsDesc.joined(separator: ", ")
    desc += ")"
    return desc
  }
}
