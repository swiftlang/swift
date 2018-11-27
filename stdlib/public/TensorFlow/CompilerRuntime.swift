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
// Design notes on TF eager based runtime (non-eager path is to be removed):
//
// 1. A global context (`_ExecutionContext.global`) is used to manage all tensor
// computation and transfers.
//
// 2. When the tensor computation involves N device functions, run them in N
// threads (but with the same execution/eager context). In addition, run the
// host-side of sends/recvs in the main thread. These N + 1 threads form
// "coroutines", and use sends/recvs mechanisms to communicate and unblock each
// other's progress.
// 2a) The sends/recvs mechanism between host and TF is the enqueue / dequeue
// operations on TF (CPU-based) Fifo queues. Only tensor handles are transferred
// in these enqueue / dequeue operations. For host to consume the content of the
// tensor sent from TF, it uses TFE_TensorHandleResolve().
// 2b) The sends/recvs mechanism between TF devices is the _Send / _Recv TF ops.
//
// Potential TODOs:
// - Support async on platforms other than Linux and FreeBSD.
// - Revisit the concurrency model and see if Dispatch can be built without
//   Foundation.
//
// NOTE:
// - Much code is intentionally un-Swifty because TF/TFE support is likely to
//   change. Variable pairs with versions for TF/TFE would be better represented
//   as an enum, but since the TFE runtime is soon to be removed it doesn't make
//   sense temporarily change the code.
// - Code should be made Swifty after support is stabilized and churn rate is
//   lower (e.g. variable pairs should be rewritten as an enums).
//
//===----------------------------------------------------------------------===//

#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
import Darwin
#else
import Glibc
#endif
import CTensorFlow

// If `serverAddress` is nil, use local session (good for forge testing).
//
// FIXME: We need transparent here because deabstraction isn't inlining this
// function.  We need to inline if a callee contains tensor ops, not only if
// it takes and returns a TensorFlow value.
@_transparent
public func enableTPU(serverAddress: String? = nil, infeed: Bool = true) {
  _RuntimeConfig.executionMode = .tpu
  if let serverAddress = serverAddress {
    _RuntimeConfig.session = .remote(grpcAddress: serverAddress)
  }
  #tfop("tfc.configureTPU", enableInfeed: infeed) as Void
}

// FIXME: Extend the interface to support multiple GPU devices, and unify it
// with enableTPU() above.
@_transparent
public func enableGPU() {
  #tfop("tfc.configureGPU") as Void
}

@_transparent
public func enableCPU() {
  #tfop("tfc.configureCPU") as Void
}

@_frozen
public enum _ExecutionMode : Equatable {
  /// CPU or GPU execution.
  case auto
  /// TPU execution.
  // TODO: assess if we can pass this bit of info from compiler settings (when
  // enableTPU() is called), and avoid having this additional runtime bit.
  case tpu
  /// XLA jit-compilation backend (will use GPU when available, and otherwise
  /// CPU).
  case xla

  public var isTPU: Bool {
    switch self {
    case .tpu: return true
    default: return false
    }
  }
}

/// The configuration for the compiler runtime.
// TODO(hongm): Revisit the longer-term design.
@_frozen
public enum _RuntimeConfig {
  /// When false, tensorflow runtime will be initialized before running any
  /// tensor program in this process.
  static public var tensorFlowRuntimeInitialized = false

  /// When true, run the entire tensor computation in
  /// _TFCStartTensorComputation(), instead of running it on a separate thread.
  /// - Note: Set to true only for debugging purposes, as it has limited
  ///   functionality (e.g. no sends/recvs support).
  static public var usesSynchronousExecution = false

  /// When true, uses the TF eager C API, and TF interpreter backend.
  /// Otherwise uses the TF C API, with execution mode set below.
  // NOTE: when this is false, sends/receives for variant and resources tensors
  // are not supported and have undefined behavior.
  static public var usesTFEagerAPI = true

  /// Only defined when usesTFEagerAPI == false.
  ///
  /// For CPU and GPU execution without XLA, use the auto mode. For XLA and/or
  /// TPU execution, set the enum value accordingly.
  static public var executionMode: _ExecutionMode = .auto

  /// When true, let TensorFlow GPU memory allocation start small and grow as
  /// needed. Otherwise, The entire GPU memory region is pre-allocated.
  static public var gpuMemoryAllowGrowth = true

  /// When non-nil, run metadata (with full trace) of each session execution
  /// will be dumped to the give path.
  static public var runMetadataOutputPath: String? = nil

  /// Specifies whether the TensorFlow computation runs in a local (in-process)
  /// session, or a remote session with the specified server address (must start
  /// with "grpc://" in that case).
  @_frozen
  public enum RuntimeSession {
    case local
    case remote(grpcAddress: String)
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

  if let value = getenv("SWIFT_TENSORFLOW_USE_TPU_INFEED"),
    String(cString: value).lowercased() == "true" {
      _RuntimeConfig.executionMode = .tpu
      debugLog("Setting TPU execution with infeed from env.")
  }

  if let value = getenv("SWIFT_TENSORFLOW_SYNC_EXECUTION"),
    String(cString: value).lowercased() == "true" {
      _RuntimeConfig.usesSynchronousExecution = true
      debugLog("Using sync execution from env.")
  }

  if let value = getenv("SWIFT_TENSORFLOW_SERVER_ADDRESS") {
    let address = String(cString: value)
    debugLog("Env var SWIFT_TENSORFLOW_SERVER_ADDRESS has value \(address).")
    if address == "local" {
      _RuntimeConfig.session = .local
      debugLog("Using local TF session.")
      return
    }

    guard address.prefix(7) == "grpc://" else {
      fatalError("SWIFT_TENSORFLOW_SERVER_ADDRESS must start with 'grpc://'.")
    }
    _RuntimeConfig.session = .remote(grpcAddress: address)
    debugLog("Setting TF server address to \(address) from env.")
  }

  if let value = getenv("SWIFT_TENSORFLOW_RUN_METADATA_OUTPUT") {
    let path = String(cString: value)
    _RuntimeConfig.runMetadataOutputPath = path
    debugLog("Setting run metadata output path to \(path) from env.")
  }
}

/// Initialize the TPU system.
/// - Note: This should be called only once.
/// - Precondition: The given session must contain the given graph.
// TODO(b/77572335): Reassess how to reset TPU after execution error.
private func initializeTPU(withSession session: CTFSession, graph: CTFGraph,
                           status: CTFStatus) {
  debugLog("Initializing TPU.")
  let configOp = TF_GraphOperationByName(graph, "ConfigureDistributedTPU")
  internalConsistencyCheck(configOp != nil)
  var configNode = TF_Output(oper: configOp, index: 0)
  var dummyOutput: CTensor?
  TF_SessionRun(session, nil, nil, nil, 0, &configNode, &dummyOutput, 1, nil,
                0, nil, status)
  checkOk(status)
  TF_DeleteTensor(dummyOutput)
}

/// The host of any tensor computation.
@_fixed_layout
public final class _ExecutionContext {
  /// Global context storing all available devices, loaded functions, etc.
  public static let global: _ExecutionContext = _ExecutionContext()

  public let cpuDeviceName: String

  /// Only set when there is a usable GPU.
  public let gpuDeviceName: String?

  /// The buffer storing a serialized TensorFlow config proto.
  public let tensorFlowConfig: UnsafeMutablePointer<TF_Buffer>

  /// The TFE_Context object.
  @usableFromInline let eagerContext: CTFEContext

  // NOTE: the following properties are intentionally not implemented as an enum
  // due to high churn, *please do not refactor for Swiftiness*.
  /// The set of all loaded programs indexed by their unique address.
  /// Used when _RuntimeConfig.usesTFEagerAPI is true.
  private var loadedTFEPrograms: [UnsafeRawPointer : CTFGraph] = [:]

  /// Used when _RuntimeConfig.usesTFEagerAPI is false.
  internal typealias ProgramInstance = (graph: CTFGraph, session: CTFSession)
  private var loadedTFPrograms: [UnsafeRawPointer : ProgramInstance] = [:]

  /// The status for checking TensorFlow errors.
  private let status: CTFStatus = TF_NewStatus()

  /// The mutex for preventing potential concurrent access.
  private var mutex: pthread_mutex_t = pthread_mutex_t()

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
    if _RuntimeConfig.executionMode == .xla {
      debugLog("Enable XLA execution.")
    }
    if _RuntimeConfig.gpuMemoryAllowGrowth {
      debugLog("Allowing growth for GPU memory allocator.")
    }
    self.tensorFlowConfig = TF_CreateConfig(
      _RuntimeConfig.executionMode == .xla ? 1 : 0,
      _RuntimeConfig.gpuMemoryAllowGrowth ? 1 : 0)
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

    if case .remote(let grpcAddress) = _RuntimeConfig.session {
      debugLog("Setting up the server def to \(grpcAddress)...")
      let serverDef: UnsafeMutablePointer<TF_Buffer>! = TFE_GetServerDef(
				grpcAddress, status)
      checkOk(status)
      TFE_ContextSetServerDef(eagerContext, /*keep_alive_secs*/0,
        serverDef.pointee.data, serverDef.pointee.length, status)
      checkOk(status)
      TF_DeleteBuffer(serverDef)
    }

    // Initialize GPU device.
    // While the code here is only needed when _RuntimeConfig.executionMode is
    // set to .gpu, running it in all code paths helps keep things simple
    // (e.g. so that the cpuDeviceName property is always set.)
    let devices = TFE_ContextListDevices(eagerContext, status)
    checkOk(status)
    defer { TF_DeleteDeviceList(devices!) }

    let deviceCount = TF_DeviceListCount(devices!)
    debugLog("There are \(deviceCount) devices.")
    var deviceNames: [String : String] = [:]
    // TODO: When we use remote session, we need to set cpu device to a local
    // device.  There is no C API yet to find the local device. So, we are
    // hard-coding the value for now.
    let localCPUDeviceName = "/job:localhost/replica:0/task:0/device:CPU:0"
    var foundCPU = false
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
      if deviceType == "CPU", deviceName == localCPUDeviceName {
        foundCPU = true
      }
      deviceNames[deviceType] = deviceName
    }
    guard foundCPU else {
      fatalError("CPU should always be an available device.")
    }
    self.cpuDeviceName = localCPUDeviceName
    // This can be nil when no GPU is available.
    self.gpuDeviceName = deviceNames["GPU"]

    // Initialize the mutex.
    pthread_mutex_init(&mutex, nil)
  }

  deinit {
    debugLog("De-initializing global context.")
    // Delete all loaded programs.
    for (graph, session) in loadedTFPrograms.values {
      TF_DeleteSession(session, status)
      checkOk(status)
      TF_DeleteGraph(graph)
    }
    TFE_DeleteContext(eagerContext)
    TF_DeleteBuffer(tensorFlowConfig)
    TF_DeleteStatus(status)
    pthread_mutex_destroy(&mutex)
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

fileprivate extension _ExecutionContext {
  /// Load the graph functions of a serialized TensorFlow GraphDef binary proto
  /// into the context, if that has not been done yet. Return the graph.
  /// - Parameters:
  ///   - address: The address of the serialized program in memory.
  ///   - count: The size of the program in bytes.
  func loadProgramInBytes(_ address: UnsafeRawPointer, count: Int) -> CTFGraph {
    return sync {
      debugLog("Loading a program.")
       // If the program is already loaded, do nothing.
      if let graph = loadedTFEPrograms[address] {
        return graph
      }
      // Here we have to do a fairly awkward dance to load the graph functions
      // and populate them into the TFE_Context.  We load the program as a
      // TF_Graph, then copy the functions out of it, then copy them into the
      // TFE_Context.
      debugLog("Loading graph functions.")
      let graph = TF_NewGraph()!
      // TensorFlow loads things through TF_Buffer.  Create one that avoids
      // redundantly copying the program bytes.
      var programBuf = TF_Buffer(data: address, length: count,
                                 data_deallocator: nil)
      let graphDefOptions = TF_NewImportGraphDefOptions()
      TF_GraphImportGraphDef(graph, &programBuf, graphDefOptions, self.status)
      TF_DeleteImportGraphDefOptions(graphDefOptions)
      checkOk(self.status)
      // Now that we have all of the TF_Function objects in the graph, copy them
      // to standalone TF_Function's.
      let funcCount = TF_GraphNumFunctions(graph)
      // Allocate an array to accept functions.
      var funcs: [CTFFunction?] = Array(repeating: nil, count: Int(funcCount))
      TF_GraphGetFunctions(graph, &funcs, funcCount, self.status)
      checkOk(self.status)

      // Add functions to the context.
      debugLog("Adding \(funcCount) functions to context.")
      for function in UnsafeBufferPointer(start: funcs, count: Int(funcCount)) {
        TFE_ContextAddFunction(self.eagerContext, function, self.status)
        checkOk(self.status)
        debugLog("Added func \(String(cString: TF_FunctionName(function))).")
        TF_DeleteFunction(function)
      }

       // Memorize the loaded program by address.
      loadedTFEPrograms[address] = graph
      debugLog("Done loading a new program.")
      return graph
    }
  }

  /// Returns a cached session along with its graph if it exists, or creates a
  /// new one and caches it.
  func session(
    forProgram programByteAddress: UnsafeRawPointer,
    programByteCount: Int
  ) -> ProgramInstance {
    return sync {
      // If a program instance for this program is already cached, use that.
      if let instance = loadedTFPrograms[programByteAddress] {
        return instance
      }

      // Otherwise, load the graph, create a session, and cache them.
      debugLog("Loading a tensor program as a TF graph.")
      let newGraph = TF_NewGraph()!
      // TensorFlow loads things through TF_Buffer.  Create one that avoids
      // redundantly copying the program bytes.
      var programBuf = TF_Buffer(data: programByteAddress,
                                 length: programByteCount,
                                 data_deallocator: nil)
      let graphDefOptions = TF_NewImportGraphDefOptions()
      TF_GraphImportGraphDef(newGraph, &programBuf, graphDefOptions, status)
      TF_DeleteImportGraphDefOptions(graphDefOptions)
      checkOk(status)
      debugLog("Done loading a new program.")

      // Prepare session options for initializing a session.
      let sessionOptions = TF_NewSessionOptions()
      TF_SetConfig(sessionOptions,
                   tensorFlowConfig.pointee.data,
                   tensorFlowConfig.pointee.length,
                   status)
      checkOk(status)

      // If needed, enable remote execution in session options.
      if case .remote(let grpcAddress) = _RuntimeConfig.session {
        debugLog("Set TensorFlow server to \(grpcAddress).")
        TF_SetTarget(sessionOptions, grpcAddress)
      }
      // Create a new session using the session options above.
      let maybeNewSession = TF_NewSession(newGraph, sessionOptions, status)
      checkOk(status)
      let newSession = maybeNewSession!
      TF_DeleteSessionOptions(sessionOptions)
      // If this is the first session in TPU mode, make sure TPU is
      // reset/initialized.
      if loadedTFPrograms.isEmpty, _RuntimeConfig.executionMode.isTPU {
        initializeTPU(withSession: newSession, graph: newGraph, status: status)
      }

      // Cache the session.
      let newInstance = (graph: newGraph, session: newSession)
      loadedTFPrograms[programByteAddress] = newInstance
      return newInstance
    }
  }
}

public extension _ExecutionContext {
  /// Load functions from a graph encoded as a byte-pointer and length into the
  /// tensorflow eagerContext.
  func loadFunctionsFromGraph(byteAddress: UnsafeRawPointer, byteCount: Int) {
    _ = loadProgramInBytes(byteAddress, count: byteCount)
  }
}

@usableFromInline
internal func dumpTensorContent<Scalar : _TensorFlowDataTypeCompatible>(
  _ inputTensor: CTensorHandle, _: Scalar.Type
) {
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

private class TFEState {
  let status: CTFStatus = TF_NewStatus()
  /// The set of graph functions to be concurrently executed (as TFE ops).
  ///
  /// The first one is on the primary device, handling input and output
  /// tensors. The other ones are helper functions that are executed only for
  /// their side effects (e.g. sending and receiving tensors with the primary
  /// function).
  var ops: [CTFEOp] = []
  init(_ programByteAddress: UnsafeRawPointer,
       programByteCount: Int,
       helperFunctionCount: Int,
       entryFunctionBaseNameAddress: UnsafePointer<Int8>) {
    let context = _ExecutionContext.global
    // Make sure the program is loaded into the context.
    let graph = context.loadProgramInBytes(programByteAddress,
                                           count: programByteCount)

    let entryFunctionBaseName = String(cString: entryFunctionBaseNameAddress)
    debugLog("Looking up op(s) from func base name \(entryFunctionBaseName).")
    for i in 0...helperFunctionCount {
      // Also look up in the TensorFlow program a function name (op type) based
      // on the op name. e.g. given op name "tfc_func_S4mainyycfU_.tf", return
      // op type "S4mainyycfU_.tf_CPU.device_partition". TFE ops are created by
      // the op types.
      var opName = "tfc_func_" + entryFunctionBaseName;
      if i > 0 {
        opName += "_helper_\(i-1)"
      }
      let funcNode = TF_GraphOperationByName(graph, opName)
      internalConsistencyCheck(
        funcNode != nil,
        "Cannot find func node name \(opName)"
      )

      let opType = String(cString: TF_OperationOpType(funcNode))
      debugLog("Creating a new op based on type \(opType).")
      let op: CTFEOp? = TFE_NewOp(context.eagerContext, opType, status)
      checkOk(status)
      if opType.hasSuffix("_CPU.device_partition") {
        TFE_OpSetDevice(op, context.cpuDeviceName, status)
        debugLog("Placing the op on device \(context.cpuDeviceName).")
      } else {
        // TODO: support TPU as well.
        internalConsistencyCheck(opType.hasSuffix("_GPU.device_partition"))
        internalConsistencyCheck(context.gpuDeviceName != nil)
        TFE_OpSetDevice(op, context.gpuDeviceName!, status)
        debugLog("Placing the op on device \(context.gpuDeviceName!).")
      }
      checkOk(status)
      ops.append(op!)
    }
  }
  deinit {
    for op in ops {
      TFE_DeleteOp(op)
    }
    TF_DeleteStatus(status)
  }
}

extension TFEState {
  func addInput(_ inputTensorHandle: CTensorHandle) {
    TFE_OpAddInput(ops[0], inputTensorHandle, status)
  }
}

private class TFState {
  let status: CTFStatus = TF_NewStatus()

  /// The TF_Session to execute the function.
  fileprivate let cSession: CTFSession
  /// The graph that contains the function to execute. Not owned.
  let graph: CTFGraph
  /// The input tensors.
  var inputTensors: [CTensor?] = []

  init(_ programByteAddress: UnsafeRawPointer, programByteCount: Int) {
    let context = _ExecutionContext.global
    (graph, cSession) = context.session(forProgram: programByteAddress,
                                        programByteCount: programByteCount)
  }

  deinit {
    TF_DeleteStatus(status)
  }
}

extension TFState {
  func addInput(_ inputTensorHandle: CTensorHandle) {
    // We assume the input tensors live in host memory.
    let cTensor = TFE_TensorHandleResolve(inputTensorHandle, status)
    checkOk(status)
    inputTensors.append(cTensor!)
  }

  /// Runs the tensor program. Aborts the process on error, and emits an error
  /// string to STDERR.
  /// See the comment on _TensorComputation.helperFunctionCount on the concept
  /// of a "helper function".
  func execute(_ entryFunctionBaseName: String,
               helperFunctionCount: Int,
               returnValues: inout [CTensorHandle?]) {
    let funcNodeName = "tfc_func_" + entryFunctionBaseName
    let funcNode = TF_GraphOperationByName(graph, funcNodeName)
    internalConsistencyCheck(
      funcNode != nil,
      "Cannot find func node name \(funcNodeName)"
    )
    internalConsistencyCheck(
      TF_OperationNumOutputs(funcNode) == returnValues.count
    )

    // Prepare input related parameters for TF_SessionRun().
    var inputNodeSpecs: [TF_Output] = []
    for i in 0..<inputTensors.count {
      let inputNodeName = "tfc_input_\(i)_\(entryFunctionBaseName)"
      let inputNode = TF_GraphOperationByName(graph, inputNodeName)
      internalConsistencyCheck(inputNode != nil,
        "Cannot find input node name \(inputNodeName)")
      inputNodeSpecs.append(TF_Output(oper: inputNode, index: 0))
    }

    // Prepare output related parameters for TF_SessionRun().
    var outputNodeSpecs: [TF_Output] = []
    for i in 0..<returnValues.count {
      let outputNodeName = "tfc_output_\(i)_\(entryFunctionBaseName)"
      let outputNode = TF_GraphOperationByName(graph, outputNodeName)
      internalConsistencyCheck(outputNode != nil,
                               "Cannot find output node name \(outputNodeName)")
      outputNodeSpecs.append(TF_Output(oper: outputNode, index: 0))
    }
    var outputTensors: [CTensor?] = Array(repeating: nil,
                                          count: returnValues.count)

    // Prepare target related parameters for TF_SessionRun().
    // A vector of TF_Operation* objects.
    var targetNodeSpecs: [OpaquePointer?] = []
    for i in 0..<helperFunctionCount {
      let helperFuncNodeName = "tfc_func_\(entryFunctionBaseName)_helper_\(i)"
      let helperFuncNode = TF_GraphOperationByName(graph, helperFuncNodeName)
      guard helperFuncNode != nil else {
        fatalError("Cannot find helper func node name \(helperFuncNodeName)")
      }
      targetNodeSpecs.append(helperFuncNode)
    }

    if returnValues.isEmpty {
        debugLog("""
          Function \(entryFunctionBaseName) has no result tensors, so adding \
          it as a target node.
          """)
      targetNodeSpecs.append(funcNode)
    }
    if _RuntimeConfig.executionMode.isTPU {
      debugLog("Enable TPU execution.")
      // When infeed is enabled, run it along with the output tensor nodes
      // below.
      let infeedEnqueueNode = TF_GraphOperationByName(graph,
                                                      "InfeedEnqueueTuple")
      if let infeedEnqueueNode = infeedEnqueueNode {
        targetNodeSpecs.append(infeedEnqueueNode)
        debugLog("Running enqueue with \(inputTensors.count) input tensors.")
      }
    }
    var runOptions: UnsafeMutablePointer<TF_Buffer>? = nil
    var runMetadataOutput: UnsafeMutablePointer<TF_Buffer>? = nil
    // When there's a run metadata output path specified, we enable `FULL_TRACE`
    // in run options and dump that to the specified path after execution.
    if _RuntimeConfig.runMetadataOutputPath != nil {
      runOptions = TF_CreateRunOptions(/*enable_full_trace*/ 1)
      runMetadataOutput = TF_NewBuffer()
    }
    debugLog("""
      Calling TF_SessionRun on function \(entryFunctionBaseName), With \
      \(targetNodeSpecs.count) target nodes.
      """)
    TF_SessionRun(
      cSession, UnsafePointer(runOptions),
      // input related parameters
      inputNodeSpecs, inputTensors, Int32(inputTensors.count),
      // output related parameters
      outputNodeSpecs, &outputTensors, Int32(returnValues.count),
      // target related parameters
      targetNodeSpecs, Int32(targetNodeSpecs.count),
      /*run_metadata*/ runMetadataOutput, status
    )
    if (TF_GetCode(status) != TF_OK) {
      _ = fputs(TF_Message(status), stderr)
      exit(-1)
    }
    debugLog("Done running TF computation.")

    // If run metadata path was set, dump the run metadata proto to a file.
    if let path = _RuntimeConfig.runMetadataOutputPath {
      TF_DeleteBuffer(runOptions)
      debugLog("Writing run metadata to \"\(path)\".")
      writeContents(of: runMetadataOutput!, toFile: path)
      TF_DeleteBuffer(runMetadataOutput)
    }

    // Delete input tensors.
    for inputTensor in inputTensors {
      TF_DeleteTensor(inputTensor)
    }

    // Synthesize TFE tensor handles to work with the existing Swift TF
    // library code.
    for i in 0..<returnValues.count {
      assert(outputTensors[i] != nil)
      returnValues[i] = TFE_NewTensorHandle(outputTensors[i], status)
      checkOk(status)
      TF_DeleteTensor(outputTensors[i])
      if _RuntimeConfig.printsDebugLog {
        dumpCTensorHandleContent(i, returnValues[i]!)
      }
    }
  }
}

//===----------------------------------------------------------------------===//
// - MARK: Tensor computation
//===----------------------------------------------------------------------===//

/// Tensor computation.
///
/// - Note: The call sequence for the APIs below must be one of the two:
///   `init -> terminate()` or `init -> finish()`.
///   The finish/terminate APIs may only be called once.
@_fixed_layout
public final class _TensorComputation {
  /// The status for checking TensorFlow errors.
  let status: CTFStatus = TF_NewStatus()

  /// The base name for the set of graph functions to execute in this instance.
  let entryFunctionBaseName: String

  /// The number of helper functions associated with this TF graph
  /// execution. For a graph execution over N TF devices, there is a single
  /// *primary* graph function, responsible for taking input and producing
  /// output tensors as part of the SessionRun() spec. The other N-1 graph
  /// functions are *helper* functions, executed together with the primary one
  /// for their side-effects, such as sending/receiving tensors, "helping" the
  /// primary function produce the desired output tensors and side effects as
  /// needed by the SessionRun() call.
  let helperFunctionCount: Int

  /// The tensor handles returned by the tensor program.
  // TODO(hongm): Retire returnValues when eager based runtime is removed.
  var returnValues: [CTensorHandle?]

  // NOTE: the following properties are intentionally not implemented as an enum
  // due to high churn, *please do not refactor for Swiftiness*.
  private var stateTFE: TFEState?
  private var stateTF: TFState?

  /// The threads to run tensor computation in. In eager mode, we use N threads
  /// when the tensor computation involves N device functions. In non-eager
  /// mode, we use a single thread (that's sufficient as these N device
  /// functions can be sent to the same TF_SessionRun() call.)
  ///
  /// The global config flag '_RuntimeConfig.usesSynchronousExecution' decides
  /// whether tensor computation should be synchronous: if true, this property
  /// will always be empty. Otherwise, this property is non-empty only when the
  /// tensor computation is on-going.
  /// TODO: Remove the `usesSynchronousExecution` mode as it is very limiting
  /// (e.g. does not support running multiple device functions).
  private var pthreads: [pthread_t] = []

  /// The data structure to pass into pthread creation API.
  /// We cannot have the ThreadBody closure below close over on `threadIndex`,
  /// because ThreadBody is of C convention.
  private class ThreadParam {
    let computation: _TensorComputation
    let threadIndex: Int

    init(computation: _TensorComputation, threadIndex: Int) {
      self.computation = computation
      self.threadIndex = threadIndex
    }
  }

  /// Loads the TF program from a binary TF FunctionDef proto given by
  /// 'programByteAddress' and 'programByteCount', and start the computation.
  ///
  /// - Parameters:
  ///   - programByteAddress: The address of the raw program.
  ///   - programByteCount: The number of bytes in the program.
  ///   - entryFunctionBaseNameAddress: The base name of the functions to run.
  ///   - tensorArgumentAddress: The address to the buffer containing tensor
  ///     arguments as CTensorHandle.
  ///   - tensorArgumentCount: The number of tensor arguments to pass in.
  ///   - helperFunctionCount: The number of helper functions to run.
  ///   - resultCount: The number of output tensors.
  ///
  /// - TODO(clattner): `resultCount` should go away when the runtime is
  ///   implemented with an async design.
  @usableFromInline
  init(programByteAddress: UnsafeRawPointer,
       programByteCount: Int,
       entryFunctionBaseNameAddress: UnsafePointer<Int8>,
       tensorArgumentAddress: UnsafePointer<CTensorHandle>,
       tensorArgumentCount: Int,
       helperFunctionCount: Int,
       resultCount: Int) {
    configureRuntimeFromEnvironment()

    let inputTensorHandles = UnsafeBufferPointer(start: tensorArgumentAddress,
                                                 count: tensorArgumentCount)

    // Initialize global execution context if that's not yet done. It caches all
    // our tensor programs.
    _ = _ExecutionContext.global

    if _RuntimeConfig.printsDebugLog {
      let buffer = UnsafeBufferPointer(
        start: programByteAddress.assumingMemoryBound(to: UInt8.self),
        count: programByteCount)
      debugLog("The program bytes are \(Array(buffer)).")
    }

    // Now that we have them in our context, we can get ready to get the top
    // level function and create an op.
    self.entryFunctionBaseName = String(cString: entryFunctionBaseNameAddress)
    debugLog("""
      Creating a new op with func base name \
      \(String(cString: entryFunctionBaseNameAddress)).
      """)
    self.helperFunctionCount = helperFunctionCount
    if _RuntimeConfig.usesTFEagerAPI {
      self.stateTFE = TFEState(
        programByteAddress,
        programByteCount: programByteCount,
        helperFunctionCount: helperFunctionCount,
        entryFunctionBaseNameAddress: entryFunctionBaseNameAddress)
      debugLog("Done initializing TFE-specific state.")
    } else {
      self.stateTF = TFState(programByteAddress,
        programByteCount: programByteCount)
      debugLog("Done initializing TF-specific state.")
    }

    debugLog("Populating the op's input list.")
    for (i, inputTensorHandle) in inputTensorHandles.enumerated() {
      if _RuntimeConfig.printsDebugLog {
        dumpCTensorHandleContent(i, inputTensorHandle)
      }

      if let stateTFE = stateTFE {
        internalConsistencyCheck(_RuntimeConfig.usesTFEagerAPI)
        stateTFE.addInput(inputTensorHandle)
      } else {
        internalConsistencyCheck(!_RuntimeConfig.usesTFEagerAPI)
        guard let stateTF = stateTF else {
          fatalError("""
            stateTF must be defined when _RuntimeConfig.usesTFEagerAPI == false.
            """)
        }
        stateTF.addInput(inputTensorHandle)
      }
    }

    debugLog("Created returning info.")
    self.returnValues = Array(repeating: nil, count: resultCount)

    debugLog("Starting TF graph execution.")

    // If it's asynchronous, we execute the tensor computation via threads.
    if !_RuntimeConfig.usesSynchronousExecution {
      let threadCount =
        _RuntimeConfig.usesTFEagerAPI ? helperFunctionCount + 1 : 1
      for threadIndex in 0..<threadCount {
        // The function to launch in the parallel thread.
#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
        typealias ThreadBody = @convention(c)
          (UnsafeMutableRawPointer) -> UnsafeMutableRawPointer?
#else
        typealias ThreadBody = @convention(c)
          (UnsafeMutableRawPointer?) -> UnsafeMutableRawPointer?
#endif
        let body: ThreadBody = { arg in
          // Set the cancelability of the detached thread.
          pthread_setcanceltype(Int32(PTHREAD_CANCEL_DEFERRED), nil)
          // Execute the tensor computation.
#if !(os(macOS) || os(iOS) || os(watchOS) || os(tvOS))
          let arg = arg!
#endif
          let param: ThreadParam =
            Unmanaged.fromOpaque(arg).takeRetainedValue()
          param.computation.execute(threadIndex: param.threadIndex)
          checkOk(param.computation.status)
          return nil
        }
#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
        var newThread: pthread_t!
#else
        var newThread = pthread_t()
#endif
        let creationStatus = pthread_create(
          &newThread, nil, body,
          Unmanaged.passRetained(
            ThreadParam(computation: self,
                        threadIndex: threadIndex)).toOpaque()
        )
        // TODO(hongm): do error handling.
        internalConsistencyCheck(creationStatus == 0)
        pthreads.append(newThread)
      }
    }
    // If it's synchronous, we call execute() on the main thread directly.
    else {
      // Log a debug message to differentiate from async computation.
      debugLog("Running tensor computation synchronously.")
      execute(threadIndex: 0)
    }
    debugLog("Exiting _TensorComputation.init().")
  }

  deinit {
    debugLog("De-initializing _TensorComputation.")
    TF_DeleteStatus(status)
  }
}

private extension _TensorComputation {
  /// In eager mode, runs a device function in a corresponding thread given by
  /// `threadIndex`. Otherwise, runs all devices functions of the tensor
  /// program. Aborts the process on error, and emits an error string to STDERR.
  // NOTE: This is to be called by the initializer. The computation gets
  // executed on initialization, thus this method will not be exposed to users.
  private func execute(threadIndex: Int) {
    debugLog("Executing thread \(threadIndex).")
    if let stateTFE = stateTFE {
      internalConsistencyCheck(_RuntimeConfig.usesTFEagerAPI)
      internalConsistencyCheck(threadIndex <= helperFunctionCount)
      let op = stateTFE.ops[threadIndex]
      if threadIndex == 0 {
        var returnValueCount = Int32(returnValues.count)
        TFE_Execute(op, &returnValues, &returnValueCount, status)
        debugLog("""
          returnValues.count=\(returnValues.count), \
          returnValueCount=\(returnValueCount).
          """)
        internalConsistencyCheck(returnValueCount == returnValues.count)
      } else {
        var returnValueCountForHelper: Int32 = 0
        TFE_Execute(op, /*returnValues*/nil, &returnValueCountForHelper, status)
        internalConsistencyCheck(returnValueCountForHelper == 0)
      }
      checkOk(status)

      debugLog("Done execution with eager.")
      return
    }
    // Non-eager based execution.
    internalConsistencyCheck(!_RuntimeConfig.usesTFEagerAPI)
    internalConsistencyCheck(threadIndex == 0)
    debugLog("Executing TF function \(entryFunctionBaseName).")
    guard let stateTF = stateTF else {
      fatalError("stateTF must be defined in non-eager mode.")
    }
    stateTF.execute(entryFunctionBaseName,
                    helperFunctionCount: helperFunctionCount,
                    returnValues: &returnValues)
    debugLog("Done execution.")
  }
}

public extension _TensorComputation {
  /// Terminates the computation, and clean up the state.
  func terminate() {
    for pthread in pthreads {
      // TODO(hongm): Assess TF's thread cancel support.
      let cancelStatus = pthread_cancel(pthread)
      internalConsistencyCheck(cancelStatus == 0)
    }
    pthreads.removeAll()
  }

  /// Waits for completion the computation as given by 'program', and returns
  /// output handles, whose underlying tensors may live on CPU or GPU.
  /// Aborts the process on error, and emits an error string to STDERR.
  func finish() -> [CTensorHandle] {
    debugLog("Calling _TensorComputation.finish().")
    if pthreads.isEmpty {
      internalConsistencyCheck(
        _RuntimeConfig.usesSynchronousExecution, """
          finish() is called in async execution mode with pthread == nil -- \
          Was finish() already called?
          """)
    }
    for pthread in pthreads {
      debugLog("Waiting for thread to join.")
      let joinStatus = pthread_join(pthread, nil)
      internalConsistencyCheck(joinStatus == 0)
    }
    pthreads.removeAll()
    debugLog("Done executing TF graph.")

    // Now that all the elements have been filled in, remove a level of
    // optional.
    return returnValues.map { $0! }
  }
}

extension _TensorComputation {
  @usableFromInline
  var cSession: CTFSession {
    if let stateTF = stateTF {
      return stateTF.cSession
    }
    fatalError("No TF Session is available!")
  }
}

//===----------------------------------------------------------------------===//
// - MARK: Compiler runtime entrypoints
//===----------------------------------------------------------------------===//
// These are the entrypoints that are well-known to the compiler internals.  The
// signatures and forms must not be changed without updating the compiler.  Any
// code put into the body of these functions will end up being inlined into the
// user code, so they are generally just wrappers around the implementation
// above.

/// Loads the TF computation from a binary TF FunctionDef proto given by 'bytes'
/// and 'size', start the computation, and return a _TensorComputation object as
/// a unique identifier for that computation.
///
/// - Parameters:
///   - programByteAddress: The address of the raw program.
///   - programByteCount: The number of bytes in the program.
///   - entryFunctionBaseNameAddress: The base name of the functions to run.
///   - tensorArgumentAddress: The address to the buffer containing tensor
///     arguments as CTensorHandle.
///   - tensorArgumentCount: The number of tensor arguments to pass in.
///   - helperFunctionCount: The number of helper functions to run.
///   - resultCount: The number of output tensors.
@inlinable
@_silgen_name("_swift_tfc_StartTensorComputation")
public func _TFCStartTensorComputation(
  _ programByteAddress: UnsafeRawPointer,
  _ programByteCount: Int,
  _ entryFunctionBaseNameAddress: UnsafePointer<Int8>,
  _ tensorArgumentAddress: UnsafePointer<CTensorHandle>,
  _ tensorArgumentCount: Int,
  _ helperFunctionCount: Int,
  _ resultCount: Int
) -> _TensorComputation {

  debugLog("""
    _TFCStartTensorComputation() is called with \(programByteCount) \
    program bytes, \(tensorArgumentCount) input tensors \
    \(String(cString:entryFunctionBaseNameAddress)) as the func base name, \
    \(helperFunctionCount) helper functions, and \(resultCount) output tensors.
    """)

  internalConsistencyCheck(programByteCount > 0, "Cannot run an empty graph!")

  return _TensorComputation(programByteAddress: programByteAddress,
                            programByteCount: programByteCount,
                            entryFunctionBaseNameAddress: entryFunctionBaseNameAddress,
                            tensorArgumentAddress: tensorArgumentAddress,
                            tensorArgumentCount: tensorArgumentCount,
                            helperFunctionCount: helperFunctionCount,
                            resultCount: resultCount)
}

/// Waits for completion of the computation as given by `computation`, and
/// returns results.
/// Aborts the process on error, and emits an error string to STDERR.
///
/// - Parameters:
///   - computation: The tensor computation to finish.
///   - tensorResultAddress: The address to an uninitialized buffer to accept
///     results of the computation, where the output tensors may live on CPU or
///     GPU.
///   - tensorResultCount: The number of results to accept from the computation.
/// - Note: The result address as passed in is pointing to uninitialized memory,
///   this must initialize the memory, transfering ownership of the tensor
///   handles to the caller.
@inlinable
@_silgen_name("_swift_tfc_FinishTensorComputation")
public func _TFCFinishTensorComputation(
  _ computation: _TensorComputation,
  _ tensorResultAddress: UnsafeMutablePointer<CTensorHandle>,
  _ tensorResultCount: Int
) {
  debugLog("Expecting \(tensorResultCount) output tensors.")
  let results = computation.finish()
  internalConsistencyCheck(results.count == tensorResultCount,
    "internal compiler error: result count mismatch!")
  tensorResultAddress.initialize(from: results, count: tensorResultCount)
}

/// Terminates the computation as given by 'program', and clean up the state.
///
/// - Parameters:
///   - program: The tensor program to terminate.
/// - Note: If the execution was synchronous, then this function does nothing.
@inlinable
@_silgen_name("_swift_tfc_TerminateTensorComputation")
public func _TFCTerminateTensorComputation(_ computation: _TensorComputation) {
  computation.terminate()
}

/// Registers all functions in a graph into the eager context if in eager mode.
///
/// - Parameters:
///   - programByteAddress: The address of the raw program.
///   - programByteCount: The number of bytes in the program.
@inlinable
@_silgen_name("_swift_tfc_RegisterTensorFunctions")
public func _TFCRegisterTensorFunctions(
  _ programByteAddress: UnsafeRawPointer,
  _ programByteCount: Int
) {
  _ExecutionContext.global.loadFunctionsFromGraph(byteAddress: programByteAddress,
                                                  byteCount: programByteCount)
}

/// Creates a scalar CTensorHandle value for the given data type.
///
/// - Parameters:
///   - value: The scalar value.
///   - dtype: The TF data type of the tensor handle to create.
/// - Returns: A new CTensorHandle representing the scalar.
/// - Precondition: T must conform to _TensorFlowDataTypeCompatible and 'dtype'
///   must be equal to T's corresponding data type.
/// - TODO(rxwei): Constrain T to _TensorFlowDataTypeCompatible and remove the
///   precondition. This requires the compiler to emit a call to the generic
///   function.
@inlinable
@_silgen_name("_swift_tfc_CreateCTensorHandle")
public func _TFCCreateCTensorHandle<T>(_ value: T,
                                       _ dtype: TF_DataType) -> CTensorHandle {
  // Create a new CTensor and initialize it to the scalar value.
  let tensor = TF_AllocateTensor(dtype, nil, 0, MemoryLayout<T>.stride)
  TF_TensorData(tensor).assumingMemoryBound(to: T.self).initialize(to: value)
  // Create a new CTensorHandle from the CTensor.
  let status = TF_NewStatus()
  let cTensorHandle = TFE_NewTensorHandle(tensor, status)
  checkOk(status)
  TF_DeleteStatus(status)
  TF_DeleteTensor(tensor)
  return cTensorHandle!
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

/// Initializes a TensorGroup value, taking ownership of all the tensor
/// handles in `tensorHandles`.
@usableFromInline
@_silgen_name("_swift_tfc_InitTensorGroup")
func _TFCInitTensorGroup<T : TensorGroup>(
    _ tensorHandles: UnsafeMutablePointer<CTensorHandle>
) -> T {
  return T(_owning: tensorHandles)
}

/// Allocates a buffer of CTensorHandles on the heap.
@usableFromInline
@_silgen_name("_swift_tfc_AllocateCHandleBuffer")
func _TFCAllocateCHandleBuffer(_ capacity: Int32)
    -> UnsafeMutablePointer<CTensorHandle> {
  return UnsafeMutablePointer.allocate(capacity: Int(capacity))
}

/// Deallocates a buffer of CTensorHandles.
@usableFromInline
@_silgen_name("_swift_tfc_DeallocateCHandleBuffer")
func _TFCDeallocateCHandleBuffer(
    _ buffer: UnsafeMutablePointer<CTensorHandle>
) {
  buffer.deallocate()
}

/// Returns the number of CTensorHandles in a TensorGroup of type T.
@_silgen_name("_swift_tfc_GetTensorGroupCHandleCount")
public func _TFCGetTensorGroupCHandleCount<T : TensorGroup>(
    _ type: T.Type
) -> Int32 {
  return T._tensorHandleCount
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
@_silgen_name("_swift_tfc_OpSetAttrBoolArray")
func _TFCOpSetAttrBoolArray(_ op: CTFEOp,
                            _ attrName: UnsafePointer<Int8>,
                            _ value: Array<Bool>) {
  value.map({ $0 ? UInt8(1) : UInt8(0) }).withUnsafeBufferPointer { buffer in
    TFE_OpSetAttrBoolList(op, attrName, buffer.baseAddress, Int32(buffer.count))
  }
}

@usableFromInline
@_silgen_name("_swift_tfc_OpSetAttrInt32Array")
func _TFCOpSetAttrInt32Array(_ op: CTFEOp,
                             _ attrName: UnsafePointer<Int8>,
                             _ value: Array<Int32>) {
  value.map(Int64.init).withUnsafeBufferPointer { buffer in
    TFE_OpSetAttrIntList(op, attrName, buffer.baseAddress, Int32(buffer.count))
  }
}

@usableFromInline
@_silgen_name("_swift_tfc_OpSetAttrInt64Array")
func _TFCOpSetAttrInt64Array(_ op: CTFEOp,
                             _ attrName: UnsafePointer<Int8>,
                             _ value: Array<Int64>) {
  value.withUnsafeBufferPointer { buffer in
    TFE_OpSetAttrIntList(op, attrName, buffer.baseAddress, Int32(buffer.count))
  }
}

@usableFromInline
@_silgen_name("_swift_tfc_OpSetAttrDoubleArray")
func _TFCOpSetAttrDoubleArray(_ op: CTFEOp,
                              _ attrName: UnsafePointer<Int8>,
                              _ value: Array<Double>) {
  value.map(Float.init).withUnsafeBufferPointer { buffer in
    TFE_OpSetAttrFloatList(op, attrName, buffer.baseAddress, Int32(buffer.count))
  }
}

@usableFromInline
@_silgen_name("_swift_tfc_OpSetAttrFloatArray")
func _TFCOpSetAttrFloatArray(_ op: CTFEOp,
                             _ attrName: UnsafePointer<Int8>,
                             _ value: Array<Float>) {
  value.withUnsafeBufferPointer { buffer in
    TFE_OpSetAttrFloatList(op, attrName, buffer.baseAddress, Int32(buffer.count))
  }
}

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

@usableFromInline
@_silgen_name("_swift_tfc_OpSetAttrTensorShape")
func _TFCOpSetAttrTensorShape(_ op: CTFEOp,
                              _ attrName: UnsafePointer<Int8>,
                              _ shape: TensorShape,
                              _ status: CTFStatus) {
  let dimensions: [Int64] = shape.dimensions.map(Int64.init)
  dimensions.withUnsafeBufferPointer { buffer in
    TFE_OpSetAttrShape(op, attrName, buffer.baseAddress, Int32(buffer.count),
                       status)
  }
}

@usableFromInline
@_silgen_name("_swift_tfc_OpSetAttrOptionalTensorShape")
func _TFCOpSetAttrOptionalTensorShape(_ op: CTFEOp,
                                      _ attrName: UnsafePointer<Int8>,
                                      _ optionalShape: TensorShape?,
                                      _ status: CTFStatus) {
  guard let shape = optionalShape else {
    TFE_OpSetAttrShape(op, attrName, nil, -1, status)
    return
  }
  _TFCOpSetAttrTensorShape(op, attrName, shape, status)
}

@usableFromInline
@_silgen_name("_swift_tfc_OpSetAttrTensorShapeArray")
func _TFCOpSetAttrTensorShapeArray(_ op: CTFEOp,
                                   _ attrName: UnsafePointer<Int8>,
                                   _ value: Array<TensorShape>,
                                   _ status: CTFStatus) {
  let flattenedDims = value.flatMap { $0.dimensions.map(Int64.init) }
  let ranks = value.map { $0.rank }
  setAttrShapeList(op: op, attrName: attrName, flattenedDims: flattenedDims,
                   ranks: ranks, status: status)
}

@usableFromInline
@_silgen_name("_swift_tfc_OpSetAttrOptionalTensorShapeArray")
func _TFCOpSetAttrOptionalTensorShapeArray(_ op: CTFEOp,
                                           _ attrName: UnsafePointer<Int8>,
                                           _ value: Array<TensorShape?>,
                                           _ status: CTFStatus) {
  let flattenedDims = value.flatMap { (tensorShapeOpt) -> [Int64] in
    if let tensorShape = tensorShapeOpt {
      return tensorShape.dimensions.map(Int64.init)
    }
    return []
  }
  let ranks = value.map { tensorShapeOpt -> Int32 in
    if let tensorShape = tensorShapeOpt {
      return tensorShape.rank
    }
    return -1
  }
  setAttrShapeList(op: op, attrName: attrName, flattenedDims: flattenedDims,
                   ranks: ranks, status: status)
}

/// Given dimensions and ranks in the form described below, makes the
/// appropriate call to TFE_OpSetAttrShapeList(op, attrName, ..., status).
///
/// - Parameters
///   - flattenedDims: all the shapes' dimensions concatenated together in
///     order
///   - ranks: all the shapes' ranks (-1 denotes unknown rank)
func setAttrShapeList(op: CTFEOp, attrName: UnsafePointer<Int8>,
                      flattenedDims: Array<Int64>, ranks: Array<Int32>,
                      status: CTFStatus) {
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

/// Wrapper around TFE_OpSetAttrString that handles converting the Swift Stdlib
/// String into a buffer that TFE_OpSetAttrString can read.
@usableFromInline
@_silgen_name("_swift_tfc_OpSetAttrString")
func _TFCOpSetAttrString(_ op: CTFEOp,
                         _ attrName: UnsafePointer<Int8>,
                         _ value: String) {
  value.utf8CString.withUnsafeBufferPointer { buffer in
    // utf8CString is null-terminated; TFE_OpSetAttrString wants
    // non-null-terminated.
    TFE_OpSetAttrString(op, attrName, buffer.baseAddress, buffer.count - 1)
  }
}

/// Wrapper around TFE_OpSetAttrStringList that handles converting the Swift
/// Strings into buffers that TFE_OpSetAttrStringList can read.
@usableFromInline
@_silgen_name("_swift_tfc_OpSetAttrStringArray")
func _TFCOpSetAttrStringArray(_ op: CTFEOp,
                             _ attrName: UnsafePointer<Int8>,
                             _ strings: [String]) {
  // Collect all the strings' utf8 bytes into a single array so that we can
  // address all the strings with a single
  // `flattenedStringBytes.withUnsafeBufferPointer`.
  var flattenedStringBytes: [CChar] = []
  var lengths: [Int] = []
  for string in strings {
    // Don't include the null-terminator because TFE_OpSetAttrStringList uses
    // lengths instead of null-terminators.
    let stringBytes = string.utf8CString.dropLast()
    flattenedStringBytes.append(contentsOf: stringBytes)
    lengths.append(stringBytes.count)
  }

  // Calculate the addresses of all the strings within our single buffer, and
  // then call TFE_OpSetAttrStringList.
  flattenedStringBytes.withUnsafeBufferPointer { flattenedStringBytesBuffer in
    var stringAddrs: [UnsafeRawPointer?] = []
    var currentStringAddr =
        flattenedStringBytesBuffer.baseAddress.map(UnsafeRawPointer.init)
    for length in lengths {
      stringAddrs.append(currentStringAddr)
      currentStringAddr = currentStringAddr?.advanced(by: length)
    }

    stringAddrs.withUnsafeBufferPointer { stringAddrsBuffer in
      lengths.withUnsafeBufferPointer { lengthsBuffer in
        TFE_OpSetAttrStringList(op, attrName, stringAddrsBuffer.baseAddress,
                                lengthsBuffer.baseAddress, Int32(strings.count))
      }
    }
  }
}

@usableFromInline
@_cdecl("_swift_tfc_CheckOk")
func _TFCCheckOk(_ s: CTFStatus) {
  checkOk(s)
}
