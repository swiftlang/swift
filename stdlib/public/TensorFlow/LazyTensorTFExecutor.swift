import CTensorFlow

/// The `TF_Operation *` type.
typealias CTFOperation = OpaquePointer

class TFGraphBuilder {
  enum Input {
    case single(TF_Output)
    case list([TF_Output])
  }

  let graph: CTFGraph = TF_NewGraph()

  /// A status object to pass to TF graph building operations.
  private let status: CTFStatus = TF_NewStatus()

  private var nodeCounter: Int = 0

  deinit {
    TF_DeleteGraph(graph)
    TF_DeleteStatus(status)
  }

  func newNodeName(base: String) -> String {
    let name = "\(base)_\(nodeCounter)"
    nodeCounter += 1
    return name
  }

  func updateAttribute(
    _ desc: CTFOperationDescription,
    _ name: String,
    _  attrValue: LazyTensorOperation.Attribute) {
    switch attrValue {
      case LazyTensorOperation.Attribute.TensorDataTypeValue(let value):
        TF_SetAttrType(desc, name, value._cDataType)
      case LazyTensorOperation.Attribute.BoolValue(let value):
        TF_SetAttrBool(desc, name, value ? 1 : 0)
      case LazyTensorOperation.Attribute.IntValue(let value):
        TF_SetAttrInt(desc, name, Int64(value))
      case LazyTensorOperation.Attribute.FloatValue(let value):
        TF_SetAttrFloat(desc, name, value)
      case LazyTensorOperation.Attribute.StringValue(let value): do {
        value.utf8CString.withUnsafeBufferPointer { buffer in
          // utf8CString is null-terminated; TF_SetAttrString wants
          // non-null-terminated.
          TF_SetAttrString(desc, name, buffer.baseAddress, buffer.count - 1)
        }
      }
      case LazyTensorOperation.Attribute.IntArray(let values): do {
        let values64 = values.map { Int64($0) }
        values64.withUnsafeBufferPointer { buffer in
          TF_SetAttrIntList(desc, name, buffer.baseAddress, Int32(buffer.count))
        }
      }
      case LazyTensorOperation.Attribute.ConstTensor(let value): do {
        TF_SetAttrTensor(desc, name, value, status)
      }
      default:
        assert(false, "Unhandled attribute \(name):\(attrValue)")
    }
  }

  func newTFGraphNode(
    name: String,
    attrs: [String: LazyTensorOperation.Attribute],
    inputs: [Input]) -> CTFOperation? {
    // Create a new graph node now.
    let desc: CTFOperationDescription! = TF_NewOperation(
      graph, name, newNodeName(base: name))

    // Set Attributes
    for (name, value) in attrs {
      updateAttribute(desc, name, value)
    }

    // Add Inputs
    for input in inputs {
      switch input {
        case Input.single(let singleInput):
          TF_AddInput(desc, singleInput)
        case Input.list(let inputList): do {
          inputList.withUnsafeBufferPointer { buffer in
              TF_AddInputList(desc, buffer.baseAddress, Int32(buffer.count))
          }
        }
      }
    }

    // Finalize operation.
    let graphNode = TF_FinishOperation(desc, status)
    checkOk(status)
    return graphNode!
  }

  func newTFConstNode(_ handle: TFETensorHandle) -> TF_Output {
    let cTensorHandle = handle._cTensorHandle
    let cTensor = TFE_TensorHandleResolve(cTensorHandle, status)
    checkOk(status)
    let desc = TF_NewOperation(graph, "Const", newNodeName(base: "Const"))
    checkOk(status)
    TF_SetAttrType(desc, "dtype", TFE_TensorHandleDataType(cTensorHandle))
    TF_SetAttrTensor(desc, "value", cTensor, status)
    checkOk(status)
    let constNode = TF_FinishOperation(desc, status)
    return TF_Output(oper: constNode, index: 0)
  }
}

class TFFunctionBuilder {
  struct FunctionDescription {
    let name: String
    let function: CTFFunction
    let outputCount: Int
    let outputGroups: [Int]
  }

  /// A status object to pass to TF graph building operations.
  private static let status: CTFStatus = TF_NewStatus()

  static func tfFunction(
    _ graphDescription: TFGraphDescription,
    _ tracedFunctionName: String) -> FunctionDescription {
    let graph = graphDescription.graph
    let eagerContext = _TFCGetGlobalEagerContext()
    let inputs = graphDescription.inputs
    let outputs = graphDescription.tfOutputs
    let tracedGraphFn = graphDescription.graphNodes.withUnsafeBufferPointer {
      operations -> CTFFunction in
      let base = operations.baseAddress
      let tracedGraphFn = TF_GraphToFunction(graph, tracedFunctionName,
        /*append_hash_to_fn_name*/ 0,
        /*num_opers*/ Int32(operations.count),
        /*opers*/ base,
        /*numinputs*/ Int32(inputs.count),
        /*inputs*/ inputs,
        /*noutputs*/ Int32(outputs.count),
        /*outputs*/ outputs,
        /*outputnames*/ nil,
        /*functionoptions*/ nil, "", status)
      checkOk(status)
      if _RuntimeConfig.printsDebugLog {
        var len: Int = 0
        let funcDebugStr = TF_FunctionDebugString(tracedGraphFn, &len)!
        debugLog("The traced function is:\n\(String(cString: funcDebugStr))")
        free(funcDebugStr)
        debugLog("Corresponding lazy tensor operations:\n")
        for output in graphDescription.outputs {
          debugLog("  \(output)")
        }
      }
      return tracedGraphFn!
    }
    TFE_ContextAddFunction(eagerContext, tracedGraphFn, status)

    let outputGroups = graphDescription.outputs.map { $0.outputCount }

    return FunctionDescription(
      name: tracedFunctionName,
      function: tracedGraphFn,
      outputCount: outputs.count,
      outputGroups: outputGroups
    )
  }

  static func execute(
    _ function: FunctionDescription,
    _ inputs: [TFETensorHandle]) -> [TFETensorHandle] {
    let eagerContext = _TFCGetGlobalEagerContext()
    let eagerOp: CTFEOp! = TFE_NewOp(eagerContext, function.name, status)
    defer { TFE_DeleteOp(eagerOp) }
    checkOk(status)

    let deviceName = _ExecutionContext.global.currentDeviceName
    if let deviceName = deviceName {
      debugLog("Placing the trace func on device \(deviceName).")
      TFE_OpSetDevice(eagerOp, deviceName, status)
      checkOk(status)
    }

    for input in inputs {
      TFE_OpAddInput(eagerOp, input._cTensorHandle, status)
      checkOk(status)
    }

    var returnValues = [CTensorHandle?](repeating: nil, count: function.outputCount)
    var outputReturnValueCount = Int32(function.outputCount)
    TFE_Execute(eagerOp, &returnValues, &outputReturnValueCount, status)
    checkOk(status)

    return returnValues.map  { TFETensorHandle(_owning: $0!) }
  }

  static func removeFunction(_ function: FunctionDescription) {
    let eagerContext = _TFCGetGlobalEagerContext()
    TFE_ContextRemoveFunction(eagerContext, function.name, status)
    checkOk(status)
    TF_DeleteFunction(function.function)
  }

}

class TFGraphDescription {
  var inputs: [TF_Output] = []
  var inputValues: [TFETensorHandle] = []
  var graphNodes: [CTFOperation?] = []
  var outputs: [LazyTensorOperation] = []
  var tfOutputs: [TF_Output] = []


  var graph: CTFGraph { graphBuilder.graph }
  var tfFunction: TFFunctionBuilder.FunctionDescription {
    TFFunctionBuilder.tfFunction(
      self, graphBuilder.newNodeName(base: "lazyTrace"))
  }

  private var graphBuilder = TFGraphBuilder()

  init(_ lazyOp: LazyTensorOperation) {
    // LazyTensor.onLiveOperations { let _ = formGraphNode($0) }
    let _ = formGraphNode(lazyOp)
    // graphNodes = graphNodesCache.map { $0.value }
    tfOutputs = []
    for output in outputs {
      let graphNode = formGraphNode(output)
      tfOutputs += Array((0..<output.outputCount).map {
          TF_Output(oper: graphNode, index: Int32($0)) })
    }
    graphNodesCache.removeAll()
  }

  private var graphNodesCache: [ObjectIdentifier: CTFOperation?] = [:]

  private func updateCacheAndGraphNodes(
    _ id: ObjectIdentifier, _ node: CTFOperation) {
    graphNodesCache[id] = node
    graphNodes.append(node)
  }

  private func formTFOutput(_ conc: TFETensorHandle, asConst: Bool) -> TF_Output {
    let id = ObjectIdentifier(conc)
    if let graphNode = graphNodesCache[id] {
      return TF_Output(oper: graphNode, index: 0)
    }

    if asConst {
      let result = graphBuilder.newTFConstNode(conc)
      updateCacheAndGraphNodes(id, result.oper)
      return result
    } else {
      let dtype = TensorDataType(TFE_TensorHandleDataType(conc._cTensorHandle))
      let dtypeAttr = LazyTensorOperation.Attribute.TensorDataTypeValue(dtype)
      let placeHolder = graphBuilder.newTFGraphNode(
        name: "Placeholder",
        attrs: ["dtype": dtypeAttr],
        inputs: [])

      let result = TF_Output(oper: placeHolder!, index: 0)
      inputs.append(result)
      inputValues.append(conc)
      updateCacheAndGraphNodes(id, placeHolder!)
      return result
    }
  }

  private func formTFOutput(_ lazyHandle: LazyTensor) -> TF_Output {
    switch lazyHandle.handle {
      case LazyTensor.Handle.conc(let h, let materialized):
        return formTFOutput(h, asConst: !materialized)
      case LazyTensor.Handle.sym(let lazyOp, let index, _): do {
        if let outputs = lazyOp.outputs {
          return formTFOutput(outputs[index], asConst: false)
        } else {
          return TF_Output(oper: formGraphNode(lazyOp), index: Int32(index))
        }
      }
    }
  }

  private func tfGraphBuilderInput(
    _ input: LazyTensorOperation.Input) -> TFGraphBuilder.Input {
    switch input {
      case LazyTensorOperation.Input.single(let h):
        return TFGraphBuilder.Input.single(formTFOutput(h))
      case LazyTensorOperation.Input.list(let elements):
        return TFGraphBuilder.Input.list(elements.map { formTFOutput($0) })
    }
  }

  private func formGraphNode(_ lazyOp: LazyTensorOperation) -> CTFOperation? {
    let id = ObjectIdentifier(lazyOp)
    if let graphNode = graphNodesCache[id] {
      return graphNode
    }

    if LazyTensor.isLive(lazyOp) { outputs.append(lazyOp) }

    let inputs = lazyOp.inputs.map { tfGraphBuilderInput($0) }
    let graphNode = graphBuilder.newTFGraphNode(
      name: lazyOp.name, attrs: lazyOp.attrs, inputs: inputs)
    updateCacheAndGraphNodes(id, graphNode!)
    return graphNode
  }
}

class LazyTraceDescription {
  // inputs will be "placeholder" nodes.
  var inputs: [LazyTensorOperation] = []
  var inputValues: [TFETensorHandle] = []
  var operations: [LazyTensorOperation] = []
  var outputs: [LazyTensorOperation] = []

  /// A status object to pass to TF graph building operations.
  private let status: CTFStatus = TF_NewStatus()

  init(_ lazyOp: LazyTensorOperation) {
    // LazyTensor.onLiveOperations { let _ = collectLazyOp($0) }
    let _ = collectLazyOp(lazyOp)
    lazyOpsCache.removeAll()
  }

  deinit {
    TF_DeleteStatus(status)
  }

  func debugPrint() {
    print("Trace")
    print("Inputs (\(inputs.count)):")
    for input in inputs {
      print("  \(input)")
    }
    print("Outputs (\(outputs.count)):")
    for output in outputs {
      for i in 0..<output.outputCount {
        if let alias = output.alias {
          print("  \(alias):\(i)")
        } else {
          print("  \(output.name)_\(ObjectIdentifier(output)):\(i)")
        }
      }
    }
    print("Operations (\(operations.count)):")
    for op in operations {
      print("  \(op)")
    }
  }

  private var lazyOpsCache: [ObjectIdentifier: LazyTensorOperation] = [:]

  private func updateCacheAndOperations(
    _ id: ObjectIdentifier, _ node: LazyTensorOperation) {
    lazyOpsCache[id] = node
    node.alias = "\(node.name)_\(operations.count)"
    operations.append(node)
  }

  private func newConstTensor(_ conc: TFETensorHandle) -> LazyTensor {
    let cTensorHandle = conc._cTensorHandle
    let result = LazyTensorOperation("Const", 1)
    let dtype = TensorDataType(TFE_TensorHandleDataType(cTensorHandle))
    let dtypeAttr = LazyTensorOperation.Attribute.TensorDataTypeValue(dtype)
    let cTensor = TFE_TensorHandleResolve(cTensorHandle, status)
    checkOk(status)
    result.attrs = [
      "dtype": dtypeAttr,
      "value": LazyTensorOperation.Attribute.ConstTensor(cTensor!)]
    updateCacheAndOperations(ObjectIdentifier(conc), result)
    return LazyTensor(_lazy: result, index: 0)
  }

  private func newPlaceholderTensor(_ conc: TFETensorHandle) -> LazyTensor {
    let cTensorHandle = conc._cTensorHandle
    let dtype = TensorDataType(TFE_TensorHandleDataType(cTensorHandle))
    let dtypeAttr = LazyTensorOperation.Attribute.TensorDataTypeValue(dtype)
    let placeholder = LazyTensorOperation("Placeholder", 1)
    placeholder.attrs = ["dtype": dtypeAttr]
    updateCacheAndOperations(ObjectIdentifier(conc), placeholder)
    inputs.append(placeholder)
    inputValues.append(conc)
    return LazyTensor(_lazy: placeholder, index: 0)
  }

  private func maybePromotedTensor(
    _ conc: TFETensorHandle, asConst: Bool) -> LazyTensor {
    let id = ObjectIdentifier(conc)
    if let lazyOp = lazyOpsCache[id] {
      return LazyTensor(_lazy: lazyOp, index: 0)
    }
    return asConst ? newConstTensor(conc) : newPlaceholderTensor(conc)
  }

  private func maybePromotedTensor(
    _ lazyHandle: LazyTensor) -> LazyTensor {
    switch lazyHandle.handle {
      case LazyTensor.Handle.conc(let h, let materialized):
        return maybePromotedTensor(h, asConst: !materialized)
      case LazyTensor.Handle.sym(let lazyOp, let index, _): do {
        if let outputs = lazyOp.outputs {
          return maybePromotedTensor(outputs[index], asConst: false)
        } else {
          return LazyTensor(_lazy: collectLazyOp(lazyOp), index: index)
        }
      }
    }
  }

  private func maybePromotedInput(
    _ input: LazyTensorOperation.Input) -> LazyTensorOperation.Input {
    switch input {
      case LazyTensorOperation.Input.single(let h):
        return LazyTensorOperation.Input.single(maybePromotedTensor(h))
      case LazyTensorOperation.Input.list(let elements):
        return LazyTensorOperation.Input.list(
          elements.map { maybePromotedTensor($0) })
    }
  }

  private func collectLazyOp(_ lazyOp: LazyTensorOperation) -> LazyTensorOperation {
    let id = ObjectIdentifier(lazyOp)
    if let cachedLazyOp = lazyOpsCache[id] {
      return cachedLazyOp
    }

    let newLazyOp = LazyTensorOperation(lazyOp.name, lazyOp.outputCount)
    newLazyOp.attrs = lazyOp.attrs
    newLazyOp.inputs = lazyOp.inputs.map { maybePromotedInput($0) }
    updateCacheAndOperations(id, newLazyOp)

    if LazyTensor.isLive(lazyOp) { outputs.append(newLazyOp) }

    return newLazyOp
  }
}


extension LazyTensorOperation {
  func materialized(index: Int) -> TFETensorHandle {
    return materialized()[index]
  }

  func materialized() -> [TFETensorHandle] {
    if let outputs = outputs { return outputs }

    LazyTensorOperation.materializeLiveTensors(self)

    // Our outputs should have been updated by now. Otherwise,
    // something terrible happened!
    assert(outputs != nil, "Materialization failed!")
    return outputs!
  }

  func maybeMaterializeInputs() {
    func maybeMaterialized(lazyTensor: LazyTensor) -> LazyTensor {
      let handle = lazyTensor.handle
      if case let LazyTensor.Handle.sym(lazyOp, index, _) = handle {
        if let outputs = lazyOp.outputs {
          return LazyTensor(_materialized: outputs[index])
        }
      }
      return lazyTensor
    }

    func maybeMaterialized(input: Input) -> Input {
      switch input {
      case Input.single(let h):
        return Input.single(maybeMaterialized(lazyTensor: h))
      case LazyTensorOperation.Input.list(let elements):
        return Input.list(elements.map { maybeMaterialized(lazyTensor: $0) })
      }
    }
    debugLog("Materializing inputs for \(self)")
    inputs = inputs.map { maybeMaterialized(input: $0) }
    debugLog("After: \(self)")
  }


  private static func materializeLiveTensors(_ lazyOp: LazyTensorOperation) {
    let lazyDescription = LazyTraceDescription(lazyOp)
    lazyDescription.debugPrint()

    LazyTensorOperation.materializationCallback("lazy")
    let graphDescription = TFGraphDescription(lazyOp)
    LazyTensorOperation.materializationCallback("graphdesc")
    let function = graphDescription.tfFunction
    LazyTensorOperation.materializationCallback("tffunction")
    let allOutputs = TFFunctionBuilder.execute(function, graphDescription.inputValues)
    LazyTensorOperation.materializationCallback("execute")

    // Slice up the outputs to various lazy tensors
    var start: Int = 0
    for lazyOp in graphDescription.outputs {
      let end = start + lazyOp.outputCount
      lazyOp.outputs = Array(allOutputs[start..<end])
      start = end
    }

    // On all the live operations rewrite the inputs so that we drop references
    // to the LazyTensorOperations..
    LazyTensor.onAllOperations { $0.maybeMaterializeInputs() }
    // Cleanup
    TFFunctionBuilder.removeFunction(function)
  }
}
