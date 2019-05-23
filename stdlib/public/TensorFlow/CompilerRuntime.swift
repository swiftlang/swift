//===-- CompilerRuntime.swift ---------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the Swift runtime support for TensorFlow computation.
//
// This file should only contain internal details: runtime-related public APIs
// should be defined in `Execution.swift`.
//
// A global context (`_ExecutionContext.global`) is used to manage all tensor
// computation and transfers.
//
// Potential TODOs:
// - Support async on platforms other than Linux and FreeBSD.
// - Revisit the concurrency model and see if Dispatch can be built without
//   Foundation.
//
//===----------------------------------------------------------------------===//

#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
import Darwin
#else
import Glibc
#endif
import CTensorFlow

/// TraceContext contains the state needed to build a trace graph function
/// (TF_Function). As eager ops are executed in tracing mode, their
/// corresponding nodes are added to the trace graph (via
/// `addEagerOpToGraph()`). When the trace is finalized (via `finalize()`), the
/// trace graph function can then be executed (via `execute()`) by the eager
/// runtime.
private class TraceContext {
  let status: CTFStatus = TF_NewStatus()

  /// The trace graph, which will be converted to a trace graph function
  /// (TF_Function) upon finalizing.
  let graph = TF_NewGraph()

  /// The list of inputs to the trace graph function. It starts with the inputs
  /// to the function that we trace (referred to as the "tracee function" or
  /// "tracee"), followed by possible additional inputs that correspond to
  /// concrete tensors produced within the trace function.
  ///
  /// For example, if the tracee is:
  ///
  /// struct TensorPair<T : TensorGroup, U : TensorGroup> : TensorGroup {
  ///   public var first: T
  ///   public var second: U
  /// }
  ///
  /// func foo(x: TensorPair) -> Tensor {
  ///   let y = Tensor<Float>(1.0)
  ///   return x.first + x.second + y
  /// }
  ///
  /// Then the generated trace graph function has 3 input tensors: x.first,
  /// x.second, and y.
  ///
  /// These symbolic tensors corresond to PlaceHolder nodes in the trace graph,
  /// and will be filled in when we execute the trace graph function.
  //
  // TODO: If some tensors in `x` are not used within `foo()`, they can be
  // pruned away in the inputs to the trace graph function.
  var symbolicInputs: [TF_Output] = []

  /// The outputs obtained by executing the `tracee` for computing the trace.
  var tracedOutputs: [CTensorHandle] = []

  /// The trace context object used in TF C API calls that convert eager ops to
  /// (trace) graph nodes.
  let cTraceContext: CTFETraceContext

  /// The trace graph function created by `finalize()`.
  var traceGraphFn: CTFFunction?

  /// The number of additional input tensors to the trace graph function,
  /// created from concrete intermediate tensors in the tracee, such as `y` in
  /// the code snippet above.
  var additionalInputTensorCount: Int32 = -1

  /// `dtypes` is the (flattened) list of TF_DataType of input tensors
  /// to the trace function.
  init(dtypes: [TF_DataType]) {
    debugLog("Instiantiating TraceContext with \(dtypes.count) input tensors.")
    for (i, dtype) in dtypes.enumerated() {
      let desc = TF_NewOperation(graph, "Placeholder", "input_\(i)")
      TF_SetAttrType(desc, "dtype", dtype)
      let result = TF_FinishOperation(desc, status)
      checkOk(status)
      symbolicInputs.append(TF_Output(oper: result, index: 0))
    }
    cTraceContext = TFE_NewTraceContext(graph)
  }

  deinit {
    TFE_DeleteTraceContext(cTraceContext)
    TF_DeleteGraph(graph)
    TF_DeleteStatus(status)
  }

  func addEagerOpToGraph(_ op: CTFEOp,
                      _ retvals: UnsafeMutablePointer<OpaquePointer?>,
                      _ retvalCount: UnsafeMutablePointer<Int32>,
                      _ status: CTFStatus) {
    TFE_AddEagerOpToGraph(op, cTraceContext, retvals, retvalCount, status)
    checkOk(status)
  }

  /// Finalize the trace graph function.
  func finalize(traceeBasicName: String) {
    internalConsistencyCheck(traceGraphFn == nil)
    var symbolicOutputs: [TF_Output] = []
    // Only add symbolic output tensors as the outputs of the trace graph function.
    // For example, let the tracee be:
    //   func foo(x: Tensor) -> (Tensor, Tensor) {
    //     let y = Tensor<Float>(1.0)
    //     return (x + x, y)
    //   }
    //
    // Here foo() returns 2 tensors, but only the first one (as computed by x +
    // x) is symbolic. The second one for y is concrete, and is computed at
    // trace creation time, not trace execution time.
    // Also see the comment block above finalizeAndExecuteTraceFn().
    for (i, tracedOutput) in tracedOutputs.enumerated()
        where TFE_TensorHandleIsConcrete(tracedOutput) == 0 {
      debugLog("Adding symbolic tracedOutput \(i) as a trace graph func output.")
      symbolicOutputs.append(TFE_GetTFOutputFromTensorHandle(tracedOutput ,status))
      checkOk(status)
    }

    let traceeInputCount = symbolicInputs.count
    // Append concrete tensors created within the tracee as symbolic inputs to
    // the generated trace graph function.
    additionalInputTensorCount = TFE_FinalizeInputTensorsFromTraceContext(
      cTraceContext)
    for i in 0..<additionalInputTensorCount {
      symbolicInputs.append(TFE_GetInputGraphNodeFromTraceContext(
                              cTraceContext, UInt32(i)))
    }

    let tracedFunctionName =
      "\(traceeBasicName)_\(_RuntimeConfig.traceGraphFunctionCounter)"
    _RuntimeConfig.traceGraphFunctionCounter += 1
    debugLog("""
               Finalizing trace graph func \(tracedFunctionName), with \
               \(traceeInputCount) tracee inputs and \
               \(additionalInputTensorCount) additional inputs, and up to \
               \(tracedOutputs.count) return values.
               """)
    traceGraphFn =
      TF_GraphToFunction(graph, tracedFunctionName,
                         /*append_hash_to_fn_name*/ 0,
                         /*num_opers*/ -1,
                         /*opers*/ nil,
                         /*numinputs*/ Int32(symbolicInputs.count),
                         /*inputs*/ symbolicInputs,
                         /*noutputs*/ Int32(symbolicOutputs.count),
                         /*outputs*/ symbolicOutputs,
                         /*outputnames*/ nil,
                         /*functionoptions*/ nil, "", status)
    checkOk(status)

    if _RuntimeConfig.printsDebugLog {
      var len: Int = 0
      let funcDebugStr = TF_FunctionDebugString(traceGraphFn, &len)!
      debugLog("The traced function is:\n\(String(cString: funcDebugStr))")
      free(funcDebugStr)
    }

    // TODO: Consider garbage-collecting these trace graph functions if we end
    // up with many of them.
    let eagerContext = _TFCGetGlobalEagerContext()
    TFE_ContextAddFunction(eagerContext, traceGraphFn, status)
    checkOk(status)
  }

  /// Execute the trace graph function, and return the list of output tensors
  /// from the trace execution. These output tensors are owned by the caller.
  func execute(
    traceeInputs: [_AnyTensorHandle], useXLA: Bool = false) -> [CTensorHandle] {
    // We must be in the `notTracing` enum mode.
    internalConsistencyCheck(_RuntimeConfig.traceState.context == nil)
    internalConsistencyCheck(traceGraphFn != nil)

    let tracedFunctionName = TF_FunctionName(traceGraphFn)
    internalConsistencyCheck(tracedFunctionName != nil)
    let eagerContext = _TFCGetGlobalEagerContext()
    let op: CTFEOp! = TFE_NewOp(eagerContext, tracedFunctionName, status)
    defer { TFE_DeleteOp(op) }
    checkOk(status)

    let deviceName = _ExecutionContext.global.currentDeviceName
    if let deviceName = deviceName {
      debugLog("Placing the trace func on device \(deviceName).")
      TFE_OpSetDevice(op, deviceName, status)
      checkOk(status)
    }

    if useXLA {
      debugLog("Enabling XLA compilation")
      TFE_OpSetAttrBool(op, "_XlaCompile", 1)
    }

    debugLog("Adding \(traceeInputs.count) tracee input tensors.")
    internalConsistencyCheck(symbolicInputs.count == traceeInputs.count
                               + Int(additionalInputTensorCount))
    for input in traceeInputs {
      _TFCOpAddInputFromTensorHandle(op, input, status)
      checkOk(status)
    }

    debugLog("Adding \(additionalInputTensorCount) additional input tensors.")
    for i in 0..<additionalInputTensorCount {
      let input = TFE_ConsumeInputConcreteTensorFromTraceContext(cTraceContext,
                                                                 UInt32(i))
      internalConsistencyCheck(input != nil)
      debugLog("""
                 Adding additional input tensor of idx \
                 \(traceeInputs.count+Int(additionalInputTensorCount)):\
                 \(input!).
                 """)
      TFE_OpAddInput(op, input, status)
      checkOk(status)
    }

    // Tell TensorFlow to execute the graph function we built, containing
    // the trace.
    let maxReturnValueCount = tracedOutputs.count
    debugLog("""
               Executing trace func \(tracedFunctionName!) with up to \
               \(maxReturnValueCount) return values.
               """)
    var returnValues = [CTensorHandle?](repeating: nil,
                                        count: maxReturnValueCount)
    var outputReturnValueCount = Int32(maxReturnValueCount)
    TFE_Execute(op, &returnValues, &outputReturnValueCount, status)
    checkOk(status)
    debugLog("""
               returnValues.count=\(returnValues.count), \
               outputReturnValueCount=\(outputReturnValueCount).
               """)
    internalConsistencyCheck(outputReturnValueCount <= returnValues.count)

    // Now that all the output elements have been filled in, remove a level of
    // optional, and also add concrete outputs.
    var traceGraphOutputs: [CTensorHandle] = []
    // Points to an element in `returnValues`.
    var returnValueIdx = 0
    // See the comment block within finalize() below on why we handle concrete
    // and symbolic output tensors differently.
    for tracedOutput in tracedOutputs {
      if TFE_TensorHandleIsConcrete(tracedOutput) != 0 {
        // These concrete tensors are owned by some other objects, so we make a
        // copy here.
        let newOutput = TFE_TensorHandleCopySharingTensor(tracedOutput, status)
        checkOk(status)
        internalConsistencyCheck(newOutput != nil)
        traceGraphOutputs.append(newOutput!)
      } else {
        // These symbolic tensors are produced by TFE_Execute() above, and we
        // need not make an extra copy.
        internalConsistencyCheck(returnValues[returnValueIdx] != nil)
        traceGraphOutputs.append(returnValues[returnValueIdx]!)
        returnValueIdx += 1
      }
    }
    internalConsistencyCheck(returnValueIdx == outputReturnValueCount)
    return traceGraphOutputs
  }

  /// Bind data to the inputs of the graph and return the specialized graph.
  func specializeTFFunction(with dataTensors: [CTensorHandle]) -> String {
    let specializedGraph = TF_NewGraph()!

    TF_GraphCopyFunction(
      specializedGraph, traceGraphFn, /*gradient*/ nil, status)
    checkOk(status)

    let tracedFunctionName = TF_FunctionName(traceGraphFn)
    internalConsistencyCheck(tracedFunctionName != nil)
    let tracedOpDesc = TF_NewOperation(
      specializedGraph, tracedFunctionName, "tracedFn")

    // Create and append the inputs to the graph function.
    let traceeInputs = symbolicInputs.dropLast(
      dataTensors.count + Int(additionalInputTensorCount))
    var inputs: [TF_Output] = []
    for (i, traceeInput) in traceeInputs.enumerated() {
      let desc = TF_NewOperation(specializedGraph, "Placeholder", "input_\(i)")
      TF_SetAttrType(desc, "dtype", TF_OperationOutputType(traceeInput))
      let result = TF_FinishOperation(desc, status)
      checkOk(status)
      let input = TF_Output(oper: result, index: 0)
      TF_AddInput(tracedOpDesc, input)
      inputs.append(input)
    }

    // Wire the data to the corresponding inputs of the tracedOp.
    for (i, cTensorHandle) in dataTensors.enumerated() {
      let cTensor = TFE_TensorHandleResolve(cTensorHandle, status)
      checkOk(status)
      let desc = TF_NewOperation(specializedGraph, "Const", "input_const_\(i)")
      TF_SetAttrType(desc, "dtype", TFE_TensorHandleDataType(cTensorHandle))
      TF_SetAttrTensor(desc, "value", cTensor, status)
      checkOk(status)
      let result = TF_FinishOperation(desc, status)
      checkOk(status)
      TF_AddInput(tracedOpDesc, TF_Output(oper: result, index: 0))
    }

    var nextConst = dataTensors.count
    debugLog("Adding \(additionalInputTensorCount) additional input tensors.")
    for i in 0..<additionalInputTensorCount {
      let cTensorHandle =
        TFE_ConsumeInputConcreteTensorFromTraceContext(cTraceContext, UInt32(i))
      internalConsistencyCheck(cTensorHandle != nil)
      let cTensor = TFE_TensorHandleResolve(cTensorHandle, status)
      checkOk(status)
      let desc = TF_NewOperation(
        specializedGraph, "Const", "input_const_\(nextConst)")
      nextConst += 1
      TF_SetAttrType(desc, "dtype", TFE_TensorHandleDataType(cTensorHandle))
      TF_SetAttrTensor(desc, "value", cTensor, status)
      checkOk(status)
      debugLog("""
                 Adding additional input tensor of idx \
                 \(traceeInputs.count+Int(additionalInputTensorCount)):\
                 \(cTensorHandle!).
                 """)
      let result = TF_FinishOperation(desc, status)
      checkOk(status)
      TF_AddInput(tracedOpDesc, TF_Output(oper: result, index: 0))
    }

    let tracedOp = TF_FinishOperation(tracedOpDesc, status)
    checkOk(status)

    // Set up outputs.
    var outputIndex: Int32 = 0
    var outputs: [TF_Output] = []
    // Get the symbolic outputs from the output.
    for output in tracedOutputs
      where TFE_TensorHandleIsConcrete(output) == 0 {
      outputs.append(TF_Output(oper: tracedOp, index: outputIndex))
      outputIndex += 1
    }
    // Create constants for the concrete outputs.
    for output in tracedOutputs
      where TFE_TensorHandleIsConcrete(output) != 0 {
      let cTensor = TFE_TensorHandleResolve(output, status)
      checkOk(status)
      let desc = TF_NewOperation(
        specializedGraph, "Const", "output_const_\(outputIndex)")
      TF_SetAttrType(desc, "dtype", TFE_TensorHandleDataType(output))
      TF_SetAttrTensor(desc, "value", cTensor, status);
      checkOk(status)
      let result = TF_FinishOperation(desc, status)
      checkOk(status)
      outputs.append(TF_Output(oper: result, index: outputIndex))
      outputIndex += 1
    }
    let specializedTFFuncName =
      "specialized_tffunc_\(_RuntimeConfig.traceGraphFunctionCounter)"
    _RuntimeConfig.traceGraphFunctionCounter += 1

    let tffunc =
      TF_GraphToFunction(specializedGraph, specializedTFFuncName,
                         /*append_hash_to_fn_name*/ 0,
                         /*num_opers*/ -1,
                         /*opers*/ nil,
                         /*numinputs*/ Int32(inputs.count),
                         /*inputs*/ inputs,
                         /*noutputs*/ Int32(outputs.count),
                         /*outputs*/ outputs,
                         /*outputnames*/ nil,
                         /*functionoptions*/ nil, "", status)
    checkOk(status)

    if _RuntimeConfig.printsDebugLog {
      var len: Int = 0
      let funcDebugStr = TF_FunctionDebugString(tffunc, &len)!
      debugLog("Specialized TF_Function is:\n\(String(cString: funcDebugStr))")
      free(funcDebugStr)
    }
    let eagerContext = _TFCGetGlobalEagerContext()
    TFE_ContextAddFunction(eagerContext, tffunc, status)
    checkOk(status)
    return specializedTFFuncName
  }
}

// This enum keeps track of whether we are building or executing a trace.
private enum TracingState {
  case notTracing
  case tracing(TraceContext)

  // Return nil if we are not in tracing mode.
  var context: TraceContext? {
    guard case let .tracing(context) = self else { return nil }
    return context
  }
}

/// The configuration for the compiler runtime.
// TODO(hongm): Revisit the longer-term design.
// @_frozen // SR-9739
public enum _RuntimeConfig {
  // TODO: change this and subsequent properties from static to thread local.
  fileprivate static var traceState: TracingState = .notTracing

  /// Used to create unique trace graph function names.
  fileprivate static var traceGraphFunctionCounter = 0

  /// When false, tensorflow runtime will be initialized before running any
  /// tensor program in this process.
  static public var tensorFlowRuntimeInitialized = false

  /// When true, let TensorFlow GPU memory allocation start small and grow as
  /// needed. Otherwise, The entire GPU memory region is pre-allocated.
  static public var gpuMemoryAllowGrowth = true

  /// The number of CPU devices.
  static public var cpuDeviceCount: UInt32 = 1

  /// When non-nil, run metadata (with full trace) of each session execution
  /// will be dumped to the give path.
  static public var runMetadataOutputPath: String? = nil

  /// Specifies whether the TensorFlow computation runs in a local (in-process)
  /// session, or a remote session with the specified server definition.
  // @_frozen // SR-9739
  public enum RuntimeSession {
    case local
    case remote(serverDef: String)
  }
  static public var session: RuntimeSession = .local

  /// When true, prints various debug messages on the runtime state.
  ///
  /// If the value is true when running tensor computation for the first time in
  /// the process, INFO log from TensorFlow will also get printed.
  static public var printsDebugLog = false

  /// Specifies the verbose log level in TensorFlow; a higher level prints out
  /// more log. Only meaningful when `printsDebugLog` is true, and must be
  /// within [0, 4] in that case.
  static public var tensorflowVerboseLogLevel: Int32 = 0 {
    willSet {
      debugLog("About to set tensorflowVerboseLogLevel to \(newValue)")
      guard newValue >= 0 && newValue <= 4 else {
        fatalError("Invalid tensorflowVerboseLogLevel value \(newValue)")
      }
    }
  }
}

private func configureRuntimeFromEnvironment() {
  if let value = getenv("SWIFT_TENSORFLOW_ENABLE_DEBUG_LOGGING"),
    String(cString: value).lowercased() == "true" {
      _RuntimeConfig.printsDebugLog = true
      debugLog("Turning on debug logging from env.")
  }

  if let value = getenv("SWIFT_TENSORFLOW_VERBOSE_LOG_LEVEL") {
    guard var verboseLevel = Int32(String(cString: value)) else {
      fatalError("SWIFT_TENSORFLOW_VERBOSE_LOG_LEVEL must take an int value.")
    }
    if verboseLevel > 4 {
      verboseLevel = 4
    }
    _RuntimeConfig.tensorflowVerboseLogLevel = verboseLevel
    debugLog("Setting TF logging verbose level to \(verboseLevel) from env.")
  }

  if let value = getenv("SWIFT_TENSORFLOW_SERVER_ADDRESS") {
    let address = String(cString: value)
    debugLog("Env var SWIFT_TENSORFLOW_SERVER_ADDRESS has value \(address).")
    if address == "local" {
      _RuntimeConfig.session = .local
      debugLog("Using local TF session.")
    } else {
      guard let idx = address.firstIndex(of: ":"),
         let endIdx = address.index(idx, offsetBy: 3, limitedBy: address.endIndex),
         address[idx..<endIdx] == "://" else {
        fatalError("SWIFT_TENSORFLOW_SERVER_ADDRESS must start with 'grpc://'.")
      }

      let `protocol` = address[address.startIndex..<idx]
      let target = address[endIdx..<address.endIndex]
      _RuntimeConfig.session = .remote(serverDef: """
        cluster {
          job {
            name: "localhost"
            tasks {
              key: 0
              value: "127.0.0.1:0"
            }
            tasks {
              key: 1
              value: "\(target)"
            }
          }
        }
        job_name: "localhost"
        task_index: 0
        protocol: "\(`protocol`)"
        """)
      debugLog("Setting TF server address to \(address) from env.")

      // At the moment, without TF_EAGER_REMOTE_USE_SEND_TENSOR_RPC=1,
      // running on TPUs freezes. Therefore, we set this environment variable
      // to 1 unless it's set explicitly.
      if let value = getenv("TF_EAGER_REMOTE_USE_SEND_TENSOR_RPC") {
        debugLog("TF_EAGER_REMOTE_USE_SEND_TENSOR_RPC already set:")
        debugLog(String(cString: value))
      } else {
        setenv("TF_EAGER_REMOTE_USE_SEND_TENSOR_RPC", "1", /*override*/ 0)
        debugLog("Setting TF_EAGER_REMOTE_USE_SEND_TENSOR_RPC to 1")
      }
    }
  }

  if let value = getenv("SWIFT_TENSORFLOW_RUN_METADATA_OUTPUT") {
    let path = String(cString: value)
    _RuntimeConfig.runMetadataOutputPath = path
    debugLog("Setting run metadata output path to \(path) from env.")
  }

  if let value = getenv("SWIFT_TENSORFLOW_CPU_DEVICE_COUNT") {
    guard let cpuDeviceCount = UInt32(String(cString: value)) else {
      fatalError("SWIFT_TENSORFLOW_CPU_DEVICE_COUNT must take an int value.")
    }
    _RuntimeConfig.cpuDeviceCount = cpuDeviceCount
    debugLog("Setting number of CPU devices to \(cpuDeviceCount) from env.")
  }
}

/// The host of any tensor computation.
@_fixed_layout
public final class _ExecutionContext {
  /// Global context storing all available devices, loaded functions, etc.
  public static let global: _ExecutionContext = _ExecutionContext()

  /// The buffer storing a serialized TensorFlow config proto.
  public let tensorFlowConfig: UnsafeMutablePointer<TF_Buffer>

  /// The TFE_Context object.
  @usableFromInline let eagerContext: CTFEContext

  /// The status for checking TensorFlow errors.
  private let status: CTFStatus = TF_NewStatus()

  /// The mutex for preventing potential concurrent access.
  private var mutex: pthread_mutex_t = pthread_mutex_t()

  /// List of devices available to this execution context.
  /// Devices are represented by their names in TensorFlow notation.
  /// See documentation for the top-level `withDevice` function for details
  /// about device names.
  private var deviceNames: [String] = []

  /// Stack of devices that models nested calls to withDevice/withDefaultDevice.
  /// Devices are represented by their names in TensorFlow notation.
  /// See documentation for the top-level `withDevice` function for details
  /// about device names.
  ///
  /// All TensorFlow operations will be put on the topmost device on the stack.
  /// When the stack is empty or the topmost device is `nil`, that allows
  /// TensorFlow to place operations on any device that it sees fit.
  private var deviceScopes: [String?] = []

  /// Initializes a new execution context by initializing available devices.
  @usableFromInline
  init() {
    configureRuntimeFromEnvironment()

    // Suppress TensorFlow logging, unless the user specified a log level.
    setenv("TF_CPP_MIN_LOG_LEVEL", "3", /*override*/ 0)

    debugLog("Initializing global context.")

    // Initialize the TF runtime exactly once. Only affects local execution
    // (when _RuntimeConfig.tensorFlowServer is set to "").
    if !_RuntimeConfig.tensorFlowRuntimeInitialized {
      InitTensorFlowRuntime(_RuntimeConfig.printsDebugLog ? 1 : 0,
                            _RuntimeConfig.tensorflowVerboseLogLevel)
      _RuntimeConfig.tensorFlowRuntimeInitialized = true
    }

    guard let opts = TFE_NewContextOptions() else {
      fatalError("ContextOptions object can never be nil.")
    }

    // Create TF config object.
    if _RuntimeConfig.gpuMemoryAllowGrowth {
      debugLog("Allowing growth for GPU memory allocator.")
    }
    self.tensorFlowConfig = TF_CreateConfig(
      /* enable_xla_compilation */ 0,
      _RuntimeConfig.gpuMemoryAllowGrowth ? 1 : 0,
      _RuntimeConfig.cpuDeviceCount)
    TFE_ContextOptionsSetConfig(opts,
                                tensorFlowConfig.pointee.data,
                                tensorFlowConfig.pointee.length,
                                status)
    checkOk(status)

    let ctx = TFE_NewContext(opts, status)
    checkOk(status)
    self.eagerContext = ctx!
    TFE_DeleteContextOptions(opts)
    checkOk(status)

    if case .remote(let serverDef) = _RuntimeConfig.session {
      debugLog("Setting up the server def to \(serverDef)...")
      let serverDef: UnsafeMutablePointer! =
        TFE_GetServerDef(serverDef, status)
      checkOk(status)
      TFE_ContextSetServerDef(eagerContext, /*keep_alive_secs*/0,
        serverDef.pointee.data, serverDef.pointee.length, status)
      checkOk(status)
      TF_DeleteBuffer(serverDef)
    }

    let devices = TFE_ContextListDevices(eagerContext, status)
    checkOk(status)
    defer { TF_DeleteDeviceList(devices!) }

    let deviceCount = TF_DeviceListCount(devices!)
    debugLog("There are \(deviceCount) devices.")
    for deviceId in 0..<deviceCount {
      let cDeviceName = TF_DeviceListName(devices, deviceId, status)
      checkOk(status)
      let deviceName = String(cString: cDeviceName!)
      let cDeviceType = TF_DeviceListType(devices, deviceId, status)
      checkOk(status)
      let deviceType = String(cString: cDeviceType!)
      debugLog(
        "Device \(deviceId) has type \(deviceType) and name \(deviceName)."
      )
      deviceNames.append(deviceName)
    }

    pthread_mutex_init(&mutex, nil)
  }

  deinit {
    debugLog("De-initializing global context.")
    // Delete all loaded programs.
    TFE_DeleteContext(eagerContext)
    TF_DeleteBuffer(tensorFlowConfig)
    TF_DeleteStatus(status)
    pthread_mutex_destroy(&mutex)
  }
}

// Elements in `outputs` can come from two sources:
// a) Symbolic tensors produced by tensor ops, and added as trace graph nodes.
// b) Concrete tensors produced by host code (e.g. Tensor(1.0)).
fileprivate func finalizeTraceFunction(_ name: String) -> TraceContext {
  guard let traceContext = _RuntimeConfig.traceState.context else {
    fatalError("Not in tracing mode!.")
  }
  _RuntimeConfig.traceState = .notTracing
  traceContext.finalize(traceeBasicName: name)
  return traceContext
}

private extension TensorArrayProtocol {
  // The returned handles are owned by the caller.
  var cTensorHandles: [CTensorHandle] {
    debugLog("Getting \(self._tensorHandleCount) C handles.")
    let buffer = UnsafeMutablePointer<CTensorHandle>.allocate(
      capacity: Int(self._tensorHandleCount))
    debugLog("Unpacking handles into buffer.")
    _unpackTensorHandles(into: buffer)
    let status = TF_NewStatus()
    debugLog("Copying buffer content to output handles.")
    var output: [CTensorHandle] = []
    for i in 0..<Int(_tensorHandleCount) {
      let address = buffer.advanced(by: i)
      let isConcrete = TFE_TensorHandleIsConcrete(address.pointee) != 0
      debugLog("""
                 Copying the \(i)-th C handle \(address.pointee) with \
                 concrete=\(isConcrete).
                 """)
      let newHandle = TFE_TensorHandleCopySharingTensor(address.pointee,
                                                        status)
      checkOk(status)
      internalConsistencyCheck(newHandle != nil)
      debugLog("""
                 Copying the \(i)-th C handle \(address.pointee) with \
                 concrete=\(isConcrete) to C handle create \(newHandle!).
                 """)
      output.append(newHandle!)
    }
    TF_DeleteStatus(status)
    return output
  }
}

private extension TensorGroup {
  // The tensors in `input` are NOT owned by this instance.
  init<C: Collection>(_copying input: C) where C.Element == CTensorHandle {
    assert(Self._tensorHandleCount == input.count)
    let buffer = UnsafeMutablePointer<CTensorHandle>.allocate(
      capacity: input.count)
    let status = TF_NewStatus()
    // copy input to buffer
    for (i, inputTensorHandle) in input.enumerated() {
      let address = buffer.advanced(by: i)
      // Each tensor can be symbolic (e.g. when using this API to create a
      // symbolic input instance to tracee) or concrete (e.g. when creating the
      // final output of the tracee).
      let newHandle = TFE_TensorHandleCopySharingTensor(inputTensorHandle,
                                                        status)
      checkOk(status)
      address.initialize(to: newHandle!)
    }
    TF_DeleteStatus(status)
    self.init(_owning: buffer)
  }

  init<C: Collection>(_owning input: C) where C.Element == CTensorHandle {
    assert(Self._tensorHandleCount == input.count)
    let buffer = UnsafeMutablePointer<CTensorHandle>.allocate(
      capacity: input.count)
    let status = TF_NewStatus()
    // copy input to buffer
    for (i, inputTensorHandle) in input.enumerated() {
      let address = buffer.advanced(by: i)
      address.initialize(to: inputTensorHandle)
    }
    TF_DeleteStatus(status)
    self.init(_owning: buffer)
  }
}

// TODO: Fold this protocol into TensorArrayProtocol.
// This requires that we move concrete implementation such as
// Tensor._makeInstance() to TensorGroup.swift.
public protocol _TensorArrayProtocolEnhanced : TensorArrayProtocol {
  // Create an instance based on `inputs`, which can be symbolic (e.g. when
  // creating a symbolic input to tracee) or concrete (e.g. when creating a
  // final output of executing the tracee).
  func _makeInstance<C: Collection>(owning inputs: C) -> Self
    where C.Element == CTensorHandle
}


extension _TensorArrayProtocolEnhanced {
  var _dtypes: [TF_DataType] {
    let count = Int(_tensorHandleCount)
    let buffer =
        UnsafeMutableBufferPointer<CTensorHandle>.allocate(capacity: count)
    defer { buffer.deallocate() }
    _unpackTensorHandles(into: buffer.baseAddress)
    return buffer.map { TFE_TensorHandleDataType($0) }
  }
}

/// Trace `fn` with newly created tensor handles and return a trace context.
private func _trace(
  with dtypes: [TF_DataType],
  in fn: ([CTensorHandle]) -> [CTensorHandle]
) -> TraceContext {
  debugLog("""
             Tracing over a function with \(dtypes.count) inputs.
             """)

  // Verify that we are not already tracing.
  internalConsistencyCheck(_RuntimeConfig.traceState.context == nil,
                           "Should not be in tracing mode already!")

  // Switch to tracing mode.
  let traceCtx = TraceContext(dtypes: dtypes)
  _RuntimeConfig.traceState = .tracing(traceCtx)

  // Handle inputs.
  let inputSymbolicTensors = traceCtx.symbolicInputs.map {
    TFE_NewTensorHandleFromTFOutput($0, TF_OperationOutputType($0))!
  }
  internalConsistencyCheck(inputSymbolicTensors.count == dtypes.count)

  // Run tracee to build the trace, adding ops to the trace graph function.
  // The tracee output can contain a mixture of symbolic and concrete tensors
  // (see the comment block within TraceContext.finalize()).
   debugLog("Running tracee in tracing mode.")
  traceCtx.tracedOutputs = fn(inputSymbolicTensors)

  debugLog("Finalizing trace graph function.")
  // TAP means tensor array protocol.
  let opType = "MyTraceFn_TAP"
  return finalizeTraceFunction(opType)
}

private func _graphInternal<State : _TensorArrayProtocolEnhanced,
                            Data : TensorGroup,
                            Result : TensorGroup>(
  with state: State,
  in fn: (State, Data) -> (State, Result?)
) -> (State, Data) -> (State, Result?) {
  let traceContext: TraceContext = withoutActuallyEscaping(fn) { escapableFn in
    let wrappedFn = { (inputs: [CTensorHandle]) -> [CTensorHandle] in
      let symbolicState = state._makeInstance(
        owning: inputs.dropLast(Data._typeList.count))
      let symbolicData = Data(
        _copying: inputs.dropFirst(Int(state._tensorHandleCount)))
      let (outputState, outputResult) = escapableFn(symbolicState, symbolicData)

      debugLog("Assembling output tensor handles.")
      let outputs =
        outputResult != nil
          ? (outputState.cTensorHandles + outputResult!.cTensorHandles)
          : outputState.cTensorHandles
      return outputs
    }
    let dtypes = state._dtypes + Data._typeList.map { $0._cDataType }
    return _trace(with: dtypes, in: wrappedFn)
  }
  // The result is a closure that captures and executes the trace graph
  // function in the trace context.
  return { (oldState: State, data: Data) -> (State, Result?) in
    debugLog("Running trace function over state \(oldState) and data \(data).")

    debugLog("Getting input state tensor handles.")
    let inputStateTensorHandles =  oldState.cTensorHandles
    var inputTensors = inputStateTensorHandles.map {
      _TFCCreateTensorHandleFromC($0)
    }
    debugLog("Getting input data tensor handles.")
    let inputDataTensorHandles =  data.cTensorHandles
    inputTensors.append(contentsOf: inputDataTensorHandles.map {
      _TFCCreateTensorHandleFromC($0)
    })

    debugLog("Executing trace graph function.")
    let returnValues = traceContext.execute(traceeInputs: inputTensors)

    debugLog("Creating output model instance.")
    let newState = state._makeInstance(owning: returnValues.prefix(
                                         Int(state._tensorHandleCount)))
    let resultValues = returnValues.dropFirst(Int(state._tensorHandleCount))
    let result: Result? = resultValues.isEmpty ? nil : Result(_owning: resultValues)
    return (newState, result)
  }
}

// TODO: rename this to `graph` when it's ready for end users.
public func _graph<State : _TensorArrayProtocolEnhanced,
                   Data : TensorGroup,
                   Result : TensorGroup>(
  with state: State,
  in fn: (State, Data) -> (State, Result)
) -> (State, Data) -> (State, Result) {
  let graphFunction = _graphInternal(with: state, in: fn)
  return { (state: State, data: Data) in
    let result = graphFunction(state, data)
    internalConsistencyCheck(result.1 != nil)
    return (result.0, result.1!)
  }
}

// TODO: rename this to `graph` when it's ready for end users.
public func _graph<State : _TensorArrayProtocolEnhanced,
                   Data : TensorGroup>(
  with state: State,
  in fn: (State, Data) -> State
) -> (State, Data) -> State {
  let graphFunction: (State, Data) -> (State, Tensor<Float>?) =
    withoutActuallyEscaping(fn) { escapableFn in
      let wrappedFn = {
        // The result argument needs to a type that conforms to TensorGroup.
        // We are arbitrarily picking Tensor<Float> here.
        (s: State, d: Data) -> (State, Tensor<Float>?) in
          (escapableFn(s, d), nil)
      }
      return _graphInternal(with: state, in: wrappedFn)
    }
  return { (state: State, data: Data) in graphFunction(state, data).0 }
}

/// Trace the given function `fn` and return a closure that can be used to
/// create a `TF_Function(State)` specialized for `data`.
public func _tffunc<State : _TensorArrayProtocolEnhanced,
                    Data : TensorGroup>(
  with state: State,
  in fn: (State, Data) -> State
) -> (Data) -> (String) {
  let traceContext: TraceContext = withoutActuallyEscaping(fn) { escapableFn in
    let wrappedFn = { (inputs: [CTensorHandle]) -> [CTensorHandle] in
      let symbolicState = state._makeInstance(
        owning: inputs.dropLast(Data._typeList.count))
      let symbolicData = Data(
        _copying: inputs.dropFirst(Int(state._tensorHandleCount)))
      let outputState = escapableFn(symbolicState, symbolicData)
      return outputState.cTensorHandles
    }
    let dtypes = state._dtypes + Data._typeList.map { $0._cDataType }
    return _trace(with: dtypes, in: wrappedFn)
  }
  return {
    data in traceContext.specializeTFFunction(with: data.cTensorHandles)
  }
}

// Trace the given function to generate a TF graph and return a closure
// that can be used to launch the graph.
public func _graph<In : TensorGroup, Out : TensorGroup>(
  _ fn: (In) -> Out, useXLA: Bool = false
) -> (In) -> Out {
  let traceContext: TraceContext = withoutActuallyEscaping(fn) { escapableFn in
    let wrappedFn = { (inputs: [CTensorHandle]) -> [CTensorHandle] in
      let buffer = UnsafeMutablePointer<CTensorHandle>.allocate(
        capacity: Int(inputs.count))
      var ptr = buffer
      for input in inputs {
        ptr.initialize(to: input)
        ptr = ptr.advanced(by: 1)
      }
      let symbolicIn = In(_owning: buffer)
      let symbolicOut = escapableFn(symbolicIn)
      return symbolicOut.cTensorHandles
    }
    let dtypes = In._typeList.map { $0._cDataType }
    return _trace(with: dtypes, in: wrappedFn)
  }
  // The result is a closure that captures and executes the trace graph
  // function in the trace context.
  return { (input: In) -> (Out) in
    debugLog("Running trace function over input \(input).")

    debugLog("Getting input state tensor handles.")
    let inputStateTensorHandles =  input.cTensorHandles
    let inputTensors = inputStateTensorHandles.map {
      _TFCCreateTensorHandleFromC($0)
    }
    debugLog("Executing trace graph function.")
    let returnValues = traceContext.execute(
      traceeInputs: inputTensors, useXLA: useXLA)

    debugLog("Creating output model instance.")
    return Out(_owning: returnValues)
  }
}

/// Trace the given function and return the name of the corresponding
/// `TF_Function: In -> Out` that was created.
public func _tffunc<In : TensorGroup, Out : TensorGroup>(
  _ fn: (In) -> Out
) -> String {
  let traceContext: TraceContext = withoutActuallyEscaping(fn) { escapableFn in
    let wrappedFn = {
      (inputs: [CTensorHandle]) -> [CTensorHandle] in
      let buffer = UnsafeMutablePointer<CTensorHandle>.allocate(
        capacity: Int(inputs.count))
      var ptr = buffer
      for input in inputs {
        ptr.initialize(to: input)
        ptr = ptr.advanced(by: 1)
      }
      let symbolicIn = In(_owning: buffer)
      let symbolicOut = escapableFn(symbolicIn)
      return symbolicOut.cTensorHandles
    }

    let dtypes = In._typeList.map { $0._cDataType }
    return _trace(with: dtypes, in: wrappedFn)
  }
  return traceContext.specializeTFFunction(with: [])
}

internal extension _ExecutionContext {
  /// Returns a valid TensorFlow device name such as, which corresponds to the
  /// closest enclosing `withDevice(_:perform:)` call.
  /// A return value of `nil` indicates the absence of `withDevice(_:perform:)` or the
  /// immediately enclosing withDefaultDevice() call.
  var currentDeviceName: String? {
    return deviceScopes.last ?? nil
  }

  /// See documentation for the top-level `withDevice` function.
  func withDevice<R>(_ kind: DeviceKind, _ index: UInt = 0,
                     perform body: () throws -> R) rethrows -> R {
    let name: String
    switch kind {
    case .cpu:
      name = "/job:localhost/replica:0/task:0/device:CPU:\(index)"
    case .gpu:
      name = "/job:localhost/replica:0/task:0/device:GPU:\(index)"
    case .tpu:
      // According to server def generated when you set
      // SWIFT_TENSORFLOW_SERVER_ADDRESS, the TPUs will all be on task 1.
      name = "/job:localhost/replica:0/task:1/device:TPU:\(index)"
    }
    return try withDevice(name, perform: body)
  }

  /// See documentation for the top-level `withDevice` function.
  func withDevice<R>(_ name: String,
                     perform body: () throws -> R) rethrows -> R {
    guard deviceNames.contains(name) else {
      fatalError("Device \(name) not found")
    }
    deviceScopes.append(name)
    ...
      deviceScopes.append(name)
      let result = try body()
      internalConsistencyCheck(deviceScopes.popLast() != nil)
      return result
    } else {
      fatalError("Device \(name) not found")
    }
  }

  /// See documentation for the top-level `withDefaultDevice` function.
  func withDefaultDevice<R>(perform body: () throws -> R) rethrows -> R {
    deviceScopes.append(nil)
    let result = try body()
    internalConsistencyCheck(deviceScopes.popLast() != nil)
    return result
  }
}

internal extension _ExecutionContext {
  /// Synchronously execute the body, preventing asynchronous computation from
  /// corrupting the context data.
  private func sync<Result>(
    execute body: () throws -> Result
  ) rethrows -> Result {
    let lockStatus = pthread_mutex_lock(&mutex)
    internalConsistencyCheck(lockStatus == 0)
    defer {
      let unlockStatus = pthread_mutex_unlock(&mutex)
      internalConsistencyCheck(unlockStatus == 0)
      // Create a cancellation point.
      pthread_testcancel()
    }
    return try body()
  }
}

@usableFromInline
internal func dumpTensorContent<Scalar : _TensorFlowDataTypeCompatible>(
  _ inputTensor: CTensorHandle, _: Scalar.Type
) {
  assert(TFE_TensorHandleIsConcrete(inputTensor) != 0)

  let array = ShapedArray<Scalar>(cTensorHandle: inputTensor)
  debugLog("Rank is \(array.rank), shape is \(array.shape).")
  debugLog("""
    The content of the \(array.scalars.count) scalars are: \
    \(array.scalars).
    """)
}

@usableFromInline
internal func dumpCTensorHandleContent(
  _ idx: Int,
  _ inputTensorHandle: CTensorHandle) {
  if TFE_TensorHandleIsConcrete(inputTensorHandle) == 0 {
    debugLog("Skip dumpping a symbolic tensor handle.")
    return
  }

  let dType: TF_DataType = TFE_TensorHandleDataType(inputTensorHandle)
  debugLog("Tensor \(idx) has TF data type \(dType).")
  switch dType {
  case TF_UINT8: dumpTensorContent(inputTensorHandle, UInt8.self)
  case TF_INT8: dumpTensorContent(inputTensorHandle, Int8.self)
  case TF_UINT16: dumpTensorContent(inputTensorHandle, UInt16.self)
  case TF_INT16: dumpTensorContent(inputTensorHandle, Int16.self)
  case TF_UINT32: dumpTensorContent(inputTensorHandle, UInt32.self)
  case TF_INT32: dumpTensorContent(inputTensorHandle, Int32.self)
  case TF_UINT64: dumpTensorContent(inputTensorHandle, UInt64.self)
  case TF_INT64: dumpTensorContent(inputTensorHandle, Int64.self)
  case TF_FLOAT: dumpTensorContent(inputTensorHandle, Float.self)
  case TF_DOUBLE: dumpTensorContent(inputTensorHandle, Double.self)
  case TF_BOOL: dumpTensorContent(inputTensorHandle, Bool.self)
  // TODO: Handle `TF_BFloat16`? BFloat16 does not have a host-side
  // representation and cannot be printed directly. Consider calling into TF
  // runtime.
  default: fatalError("Unsupported dtype \(dType)")
  }
}

@usableFromInline
@_cdecl("_swift_tfc_EagerExecute")
func _TFCEagerExecute(_ op: CTFEOp,
                      _ retvals: UnsafeMutablePointer<OpaquePointer?>,
                      _ retvalCount: UnsafeMutablePointer<Int32>,
                      _ status: CTFStatus) {
  if _RuntimeConfig.printsDebugLog {
    debugLog("Calling _TFCEagerExecute() over: ")
    if let value = getenv("TF_CPP_MIN_LOG_LEVEL"),
      String(cString: value) == "0" {
      TFE_OpPrintDebugString(op)
    } else {
      debugLog("[Run with TF_CPP_MIN_LOG_LEVEL=0 to have TFEOps printed out]")
    }
  }
  if let traceContext = _RuntimeConfig.traceState.context {
    // convert this eager op into a trace graph node
    debugLog("Adding eager op \(op) to trace graph.")
    traceContext.addEagerOpToGraph(op, retvals, retvalCount, status)
    checkOk(status)
  } else {
    debugLog("Executing eager op \(op).")
    TFE_Execute(op, retvals, retvalCount, status)
  }
}

//===----------------------------------------------------------------------===//
// - MARK: Dynamic compilation (per-op dispatch) entrypoints
//===----------------------------------------------------------------------===//

@usableFromInline
@_cdecl("_swift_tfc_GetGlobalEagerContext")
func _TFCGetGlobalEagerContext() -> CTFEContext {
  debugLog("Calling _GetGlobalEagerContext()")
  return _ExecutionContext.global.eagerContext
}

// Some of the functions are marked with @silgen_name instead of @_cdecl,
// because their input/output data types are not C-compatible
// (e.g. AnyTensorHandle).

/// Adds `handle` as an input to `op`.
@usableFromInline
@_silgen_name("_swift_tfc_OpAddInputFromTensorHandle")
func _TFCOpAddInputFromTensorHandle(_ op: CTFEOp,
                                    _ handle: _AnyTensorHandle,
                                    _ status: CTFStatus) {
  TFE_OpAddInput(op, handle._cTensorHandle, status)
}

/// Adds `t` as an input or inputs to `op`. Returns the number of inputs added.
@usableFromInline
@_silgen_name("_swift_tfc_OpAddInputFromTensorGroup")
func _TFCOpAddInputFromTensorGroup<T : TensorArrayProtocol>(
    _ op: CTFEOp, _ t: T, _ status: CTFStatus
) -> Int32 {
  let count = t._tensorHandleCount
  let buffer =
      UnsafeMutableBufferPointer<CTensorHandle>.allocate(capacity: Int(count))
  defer { buffer.deallocate() }
  t._unpackTensorHandles(into: buffer.baseAddress)
  for handle in buffer {
    TFE_OpAddInput(op, handle, status)
    guard TF_GetCode(status) == TF_OK else {
      return 0
    }
  }
  return count
}

/// Special protocol for calling tensorflow operations that take heterogeneous
/// arrays as input.
public protocol AnyTensor {
  var _rawTensorHandle: CTensorHandle { get }
  var _tensorFlowDataType: TensorDataType { get }
}

extension Tensor : AnyTensor {
  public var _rawTensorHandle: CTensorHandle { return handle._cTensorHandle }
  public var _tensorFlowDataType: TensorDataType { return Scalar.tensorFlowDataType }
}

@usableFromInline
func _TFCOpAddInputFromAnyTensors(
  _ op: CTFEOp, _ tensors: [AnyTensor], _ status: CTFStatus
) {
  for tensor in tensors {
    let handle = tensor._rawTensorHandle
    TFE_OpAddInput(op, handle, status)
    checkOk(status)
  }
}

@inlinable
@_silgen_name("_swift_tfc_CreateTensorHandleFromC")
public func _TFCCreateTensorHandleFromC(
  _ cHandle: CTensorHandle
) -> _AnyTensorHandle {
  let dtype = TFE_TensorHandleDataType(cHandle)
  switch dtype {
  case TF_BFLOAT16: return TensorHandle<BFloat16>(_owning: cHandle)
  case TF_UINT8: return TensorHandle<UInt8>(_owning: cHandle)
  case TF_INT8: return TensorHandle<Int8>(_owning: cHandle)
  case TF_UINT16: return TensorHandle<UInt16>(_owning: cHandle)
  case TF_INT16: return TensorHandle<Int16>(_owning: cHandle)
  case TF_UINT32: return TensorHandle<UInt32>(_owning: cHandle)
  case TF_INT32: return TensorHandle<Int32>(_owning: cHandle)
  case TF_UINT64: return TensorHandle<UInt64>(_owning: cHandle)
  case TF_INT64: return TensorHandle<Int64>(_owning: cHandle)
  case TF_FLOAT: return TensorHandle<Float>(_owning: cHandle)
  case TF_DOUBLE: return TensorHandle<Double>(_owning: cHandle)
  case TF_BOOL: return TensorHandle<Bool>(_owning: cHandle)
  case TF_STRING: return TensorHandle<String>(_owning: cHandle)
  case TF_RESOURCE: return ResourceHandle(owning: cHandle)
  case TF_VARIANT: return VariantHandle(owning: cHandle)
  default: fatalError("Unsupported dtype \(dtype)")
  }
}

// _TFCOpSetAttr*Array functions are wrappers around TFE_OpSetAttr*List
// functions. The wrappers handle converting the Swift Stdlib Array<T> values
// into buffers that TFE_OpSetAttr*List functions can read.

@usableFromInline
@_silgen_name("_swift_tfc_OpSetAttrTypeArray")
func _TFCOpSetAttrTypeArray(_ op: CTFEOp,
                            _ attrName: UnsafePointer<Int8>,
                            _ value: Array<TensorDataType>) {
  value.withUnsafeBufferPointer { buffer in
    buffer.withMemoryRebound(to: TF_DataType.self) { reboundBuffer in
      TFE_OpSetAttrTypeList(op, attrName, reboundBuffer.baseAddress,
                            Int32(reboundBuffer.count))
    }
  }
}

/// Given dimensions and ranks in the form described below, makes the
/// appropriate call to `TFE_OpSetAttrShapeList(op, attrName, ..., status)`.
///
/// - Parameters
///   - flattenedDims: all the shapes' dimensions concatenated together in
///     order
///   - ranks: all the shapes' ranks (-1 denotes unknown rank)
fileprivate func setAttrShapeList(
  op: CTFEOp, attrName: UnsafePointer<Int8>, flattenedDims: Array<Int64>,
  ranks: Array<Int32>, status: CTFStatus
) {
  flattenedDims.withUnsafeBufferPointer { flattenedDimsBuffer in
    var dimsPtr: UnsafePointer<Int64>? = flattenedDimsBuffer.baseAddress
    var dims: [UnsafePointer<Int64>?] = []
    for rank in ranks {
      dims.append(dimsPtr)
      if rank >= 0 {
        dimsPtr = dimsPtr.map { $0.advanced(by: Int(rank)) }
      }
    }
    dims.withUnsafeMutableBufferPointer { dimsBuffer in
      ranks.withUnsafeBufferPointer { ranksBuffer in
        TFE_OpSetAttrShapeList(op, attrName, dimsBuffer.baseAddress,
                               ranksBuffer.baseAddress,
                               Int32(ranksBuffer.count), status)
      }
    }
  }
}

@usableFromInline
@_cdecl("_swift_tfc_OpSetDeviceFromScope")
func _TFCOpSetDeviceFromScope(_ op: CTFEOp, _ status: CTFStatus) {
  if let deviceName = _ExecutionContext.global.currentDeviceName {
    TFE_OpSetDevice(op, deviceName, status)
  }
}
