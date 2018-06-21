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
// TODO:
// - Support async on platforms other than Linux and FreeBSD.
// - Revisit the concurrency model and see if Dispatch can be built without
//   Foundation.
// - Detach compiler runtime from the TensorFlow standard library to a separate
//   TensorFlowRuntime module.
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
  _RuntimeConfig.executionMode = .gpu
  #tfop("tfc.configureGPU") as Void
}

@_frozen
public enum _ExecutionMode : Equatable {
  /// Classical TF interpreter backend, on CPU.
  case cpu
  /// Classical TF interpreter backend, on GPU.
  case gpu
  /// TPU backend.
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
  /// - Note: Set to true only for debugging purposes.
  static public var usesSynchronousExecution = false

  /// Setting the value to gpu requires that the Swift compiler and model code
  /// be built with `--config=cuda`. If there are multiple GPUs, an arbitrary
  /// one is chosen.
  static public var executionMode: _ExecutionMode = .cpu {
    willSet {
      debugLog("About to set executionMode to \(newValue)")
      guard newValue == .gpu else { return }
      guard _ExecutionContext.global.gpuDeviceName != nil else {
        fatalError("""
          GPU must be available when _RuntimeConfig.executionMode is set to \
          .gpu -- did you compile with --config=cuda and have a qualified GPU?
          """)
      }
    }
  }

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
    debugLog("env var SWIFT_TENSORFLOW_SERVER_ADDRESS has value \(address).")
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
}

/// Initialize the TPU system.
/// - Note: This should be called only once.
/// - Precondition: The given session must contain the given graph.
// TODO(b/77572335): Reassess how to reset TPU after execution error.
private func initializeTPU(withSession session: CTFSession, graph: CTFGraph,
                           status: CTFStatus) {
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

  /// The TFE_Context object.
  private var cContext: CTFEContext

  // NOTE: the following properties are intentionally not implemented as an enum
  // due to high churn, *please do not refactor for Swiftiness*.
  internal typealias ProgramInstance = (graph: CTFGraph, session: CTFSession)
  private var loadedTFPrograms: [UnsafeRawPointer : ProgramInstance] = [:]

  /// The status for checking TensorFlow errors.
  private let status: CTFStatus = TF_NewStatus()

  /// The mutex for preventing potential concurrent access.
  private var mutex: pthread_mutex_t = pthread_mutex_t()

  /// Initializes a new execution context by initializing available devices.
  @_versioned
  init() {
    configureRuntimeFromEnvironment()

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
    let ctx = TFE_NewContext(opts, status)
    checkOk(status)
    self.cContext = ctx!
    TFE_DeleteContextOptions(opts)
    checkOk(status)

    // Initialize GPU device.
    // While the code here is only needed when _RuntimeConfig.executionMode is
    // set to .gpu, running it in all code paths helps keep things simple
    // (e.g. so that the cpuDeviceName property is always set.)
    let devices = TFE_ContextListDevices(cContext, status)
    checkOk(status)
    defer { TF_DeleteDeviceList(devices!) }

    let deviceCount = TF_DeviceListCount(devices!)
    debugLog("There are \(deviceCount) devices.")
    var deviceNames: [String : String] = [:]
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
      deviceNames[deviceType] = deviceName
    }
    guard let cpuDeviceName = deviceNames["CPU"] else {
      fatalError("CPU should always be an available device.")
    }
    self.cpuDeviceName = cpuDeviceName
    // This must be non-nil when _RuntimeConfig.executionMode is set to .gpu, as
    // being enforced in the willSet check for that property.
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
    TFE_DeleteContext(cContext, status)
    checkOk(status)
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

  /// Invokes the given closure with the underlying C context. Access to the C
  /// context is guaranteed to be thread-safe within the closure.
  func withMutableCContext<Result>(
    execute body: (CTFEContext) throws -> Result
  ) rethrows -> Result {
    return try sync {
      try body(cContext)
    }
  }
}

fileprivate extension _ExecutionContext {
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
      // If needed, enable XLA compilation in session options.
      if _RuntimeConfig.executionMode == .xla {
        debugLog("Enable XLA execution.")
        TF_EnableXLACompilation(sessionOptions, 1)
      }
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

@_versioned
internal func dumpTensorContent<Scalar : AccelerableByTensorFlow>(
  _ inputTensor: CTensorHandle, _: Scalar.Type
) {
  let array = ShapedArray<Scalar>(cTensorHandle: inputTensor)
  debugLog("Rank is \(array.rank), shape is \(array.shape).")
  debugLog("""
    The content of the \(array.scalars.count) scalars are: \
    \(array.scalars).
    """)
}

@_versioned
internal func dumpCTensorHandleContent(
  _ idx: Int,
  _ inputTensorHandle: CTensorHandle) {
  let dType: TF_DataType = TFE_TensorHandleDataType(inputTensorHandle)
  debugLog("Tensor \(idx) has TF data type \(dType).")
  switch dType {
  case TF_INT8: dumpTensorContent(inputTensorHandle, Int8.self)
  case TF_UINT8: dumpTensorContent(inputTensorHandle, UInt8.self)
  case TF_INT16: dumpTensorContent(inputTensorHandle, Int16.self)
  case TF_UINT16: dumpTensorContent(inputTensorHandle, UInt16.self)
  case TF_INT16: dumpTensorContent(inputTensorHandle, Int16.self)
  case TF_UINT32: dumpTensorContent(inputTensorHandle, UInt32.self)
  case TF_INT32: dumpTensorContent(inputTensorHandle, Int32.self)
  case TF_UINT64: dumpTensorContent(inputTensorHandle, UInt64.self)
  case TF_INT64: dumpTensorContent(inputTensorHandle, Int64.self)
  case TF_FLOAT: dumpTensorContent(inputTensorHandle, Float.self)
  case TF_DOUBLE: dumpTensorContent(inputTensorHandle, Double.self)
  case TF_BOOL: dumpTensorContent(inputTensorHandle, Bool.self)
  default: fatalError("Unsupported dtype \(dType)")
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
                   Function \(entryFunctionBaseName) has no result tensors, so \
                   adding it as a target node.
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
    debugLog("""
               Calling TF_SessionRun on function \(entryFunctionBaseName), With \
               \(targetNodeSpecs.count) target nodes.
               """)
    TF_SessionRun(
      cSession, nil,
      // input related parameters
      inputNodeSpecs, inputTensors, Int32(inputTensors.count),
      // output related parameters
      outputNodeSpecs, &outputTensors, Int32(returnValues.count),
      // target related parameters
      targetNodeSpecs, Int32(targetNodeSpecs.count),
      /*run_metadata*/nil, status
    )
    checkOk(status)
    debugLog("Done running TF computation.")

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
  private var stateTF: TFState?

  /// The thread to run tensor computation in. The global config flag
  /// '_RuntimeConfig.usesSynchronousExecution' decides whether tensor
  /// computation should be synchronous: if true, this property will always be
  /// nil.
  private var pthread: pthread_t?

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
  @_versioned
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

    // Get global execution context, which caches all our tensor programs.
    let context = _ExecutionContext.global

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
    self.stateTF = TFState(programByteAddress,
                           programByteCount: programByteCount)
    debugLog("Done initializing TF-specific state.")

    if _RuntimeConfig.executionMode == .gpu {
      guard context.gpuDeviceName != nil else {
        fatalError("""
          The availability of a GPU device should have been confirmed \
          at this point.
          """)
      }
    }

    debugLog("Populating the op's input list.")
    for (i, inputTensorHandle) in inputTensorHandles.enumerated() {
      if _RuntimeConfig.printsDebugLog {
        dumpCTensorHandleContent(i, inputTensorHandle)
      }

      guard let stateTF = stateTF else {
        fatalError("stateTF must be defined.")
      }
      stateTF.addInput(inputTensorHandle)
    }

    debugLog("Created returning info.")
    self.returnValues = Array(repeating: nil, count: resultCount)

    debugLog("Starting TF graph execution.")

    // If it's asynchronous, we start a pthread that calls execute().
    if !_RuntimeConfig.usesSynchronousExecution {
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
        let computation: _TensorComputation =
          Unmanaged.fromOpaque(arg).takeRetainedValue()
        computation.execute()
        checkOk(computation.status)
        return nil
      }
#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
      var newThread: pthread_t?
#else
      var newThread = pthread_t()
#endif
      let creationStatus = pthread_create(
        &newThread, nil, body,
        Unmanaged.passRetained(self).toOpaque()
      )
      self.pthread = newThread
      // TODO(hongm): do error handling.
      internalConsistencyCheck(creationStatus == 0)
    }
    // If it's asynchronous, we call execute() on the main thread directly.
    else {
      // Log a debug message to differentiate from async computation.
      debugLog("Running tensor computation synchronously.")
      execute()
    }
    debugLog("Exiting _TensorComputation.init().")
  }

  deinit {
    debugLog("De-initializing _TensorComputation.")
    TF_DeleteStatus(status)
  }
}

private extension _TensorComputation {
  // NOTE: This is to be called by the initializer. The computation gets
  // executed on initialization, thus this method will not be exposed to users.
  private func execute() {
    debugLog("Executing TF function \(entryFunctionBaseName).")
    guard let stateTF = stateTF else {
      fatalError("stateTF must be defined.")
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
    if let pthread = pthread {
      // TODO(hongm): Assess TF's thread cancel support.
      let cancelStatus = pthread_cancel(pthread)
      internalConsistencyCheck(cancelStatus == 0)
      self.pthread = nil
    }
  }

  /// Waits for completion the computation as given by 'program', and returns
  /// output handles, whose underlying tensors may live on CPU or GPU.
  func finish() -> [CTensorHandle] {
    debugLog("Calling _TensorComputation.finish().")
    if let pthread = pthread {
      debugLog("Waiting for thread to join.")
      let joinStatus = pthread_join(pthread, nil)
      internalConsistencyCheck(joinStatus == 0)
      self.pthread = nil
    }
    debugLog("Done executing TF graph.")

    // Now that all the elements have been filled in, remove a level of
    // optional.
    return returnValues.map { $0! }
  }
}

extension _TensorComputation {
  @_versioned
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
@_inlineable
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
@_inlineable
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
@_inlineable
@_silgen_name("_swift_tfc_TerminateTensorComputation")
public func _TFCTerminateTensorComputation(_ computation: _TensorComputation) {
  computation.terminate()
}

/// Creates a scalar CTensorHandle value for the given data type.
///
/// - Parameters:
///   - value: The scalar value.
///   - dtype: The TF data type of the tensor handle to create.
/// - Returns: A new CTensorHandle representing the scalar.
/// - Precondition: T must conform to AccelerableByTensorFlow and 'dtype' must
///   be equal to T's corresponding data type.
/// - TODO(rxwei): Constrain T to AccelerableByTensorFlow and remove the
///   precondition. This requires the compiler to emit a call to the generic
///   function.
@_inlineable
@_silgen_name("_swift_tfc_CreateCTensorHandle")
public func _TFCCreateCTensorHandle<T>(_ value : T,
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
