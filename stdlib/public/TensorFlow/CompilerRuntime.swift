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

public enum _ExecutionMode {
  /// Classical TF interpreter backend, on CPU.
  case cpu
  /// Classical TF interpreter backend, on GPU.
  case gpu
  /// TPU backend.
  case tpu
  /// XLA jit-compilation backend (will use GPU when available, and otherwise
  /// CPU).
  case xla
}

/// The configuration for the compiler runtime.
/// TODO(hongm): Revisit the longer-term design.
public enum _RuntimeConfig {
  /// When true, run the entire tensor computation in
  /// _TFCStartTensorComputation(), instead of running it on a separate thread.
  /// - Note: Set to true only for debugging purposes.
  static public var usesSynchronousExecution = false

  /// When true, uses the TF eager C API, and TF interpreter backend.
  /// Otherwise uses the TF C API, with execution mode set below.
  static public var usesTFEagerAPI = false

  /// Only defined when usesTFEagerAPI == false.
  ///
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

/// The host of any tensor computation.
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
  /// The set of all loaded programs indexed by their unique address.
  /// Used when _RuntimeConfig.usesTFEagerAPI is true.
  private var loadedTFEPrograms: Set<UnsafeRawPointer> = []
  /// Used when _RuntimeConfig.usesTFEagerAPI is false.
  private var loadedTFPrograms: [UnsafeRawPointer : CTFGraph] = [:]

  /// The status for checking TensorFlow errors.
  private let status: CTFStatus = TF_NewStatus()

#if os(Linux) || os(FreeBSD)
  /// The mutex for preventing potential concurrent access.
  private var mutex: pthread_mutex_t = pthread_mutex_t()
#endif

  /// Initializes a new execution context by initializing available devices.
  private init() {
    // A simple way to turn on debug logging via commandline arguments.
    for arg in CommandLine.arguments {
      if arg == "--enable-debuglog" {
        _RuntimeConfig.printsDebugLog = true
      } else if arg.prefix(4) == "--v=" {
        guard var verboseLevel = Int32(arg.dropFirst(4)) else {
          fatalError("Commandline argument \(arg) is not of form --v=<int>")
        }
        if verboseLevel > 4 {
          verboseLevel = 4
        }
        _RuntimeConfig.tensorflowVerboseLogLevel = verboseLevel
        debugLog("Setting TF logging verbose level to \(verboseLevel)")
      }
    }

    debugLog("Initializing global context.")

    // Initialize the TF runtime exactly once.
    InitTensorFlowRuntime(_RuntimeConfig.printsDebugLog ? 1 : 0,
                          _RuntimeConfig.tensorflowVerboseLogLevel)

    let opts = TFE_NewContextOptions()
    if _RuntimeConfig.usesTFEagerAPI {
      // This only affects GPU based tensor computation, where any input tensors
      // living in CPU will be transparently copied onto GPU.
      TFE_ContextOptionsSetDevicePlacementPolicy(opts,
                                                 TFE_DEVICE_PLACEMENT_SILENT)
    }
    cContext = TFE_NewContext(opts, status)
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
#if os(Linux) || os(FreeBSD)
    pthread_mutex_init(&mutex, nil)
#endif
  }

  deinit {
    debugLog("De-initializing global context.")

    for (_, graph) in loadedTFPrograms {
      TF_DeleteGraph(graph)
    }
    TFE_DeleteContext(cContext, status)
    checkOk(status)
    TF_DeleteStatus(status)
#if os(Linux) || os(FreeBSD)
    pthread_mutex_destroy(&mutex)
#endif
  }
}

public extension _ExecutionContext {
  /// Remove all cached TensorFlow programs.
  /// - FIXME: This is temporarily added so that runtime tests can pass while
  ///   still using the old protobufs with "the_function" as the name of the
  ///   entry function.
  func reset() {
    sync { [unowned self] in
      // Delete the current context and create a new context.
      TFE_DeleteContext(self.cContext, self.status)
      checkOk(self.status)
      let opts = TFE_NewContextOptions()
      self.cContext = TFE_NewContext(opts, self.status)
      TFE_DeleteContextOptions(opts)
      checkOk(self.status)
    }
  }
}

internal extension _ExecutionContext {
  /// Synchronously execute the body, preventing asynchronous computation from
  /// corrupting the context data.
  private func sync<Result>(
    execute body: () throws -> Result
  ) rethrows -> Result {
#if os(Linux) || os(FreeBSD)
    let lockStatus = pthread_mutex_lock(&mutex)
    internalConsistencyCheck(lockStatus == 0)
    defer {
      let unlockStatus = pthread_mutex_unlock(&mutex)
      internalConsistencyCheck(unlockStatus == 0)
      // Create a cancellation point.
      pthread_testcancel()
    }
#endif // Async mode does not support other platforms, so it's already sync.
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
  /// Load a serialized TensorFlow program in binary proto format to the
  /// context. If the program has already been loaded, this function does
  /// nothing.
  /// - Parameters:
  ///   - address: The address of the serialized program in memory.
  ///   - count: The size of the program in bytes.
  func loadProgramInBytes(_ address: UnsafeRawPointer, count: Int) {
    sync { [unowned self] in
      debugLog("Loading a program.")

      // If the program is already loaded, do nothing.
      if loadedTFEPrograms.contains(address) {
        return
      }

      // Here we have to do a fairly awkward dance to load the graph functions
      // and populate them into the TFE_Context.  We load the program as a
      // TF_Graph, then copy the functions out of it, then copy them into the
      // TFE_Context.
      debugLog("Loading graph functions.")
      let graph = TF_NewGraph()
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
      // Delete the graph as it's no longer needed.
      TF_DeleteGraph(graph)

      // Add functions to the context.
      debugLog("Adding functions to context.")
      for function in UnsafeBufferPointer(start: funcs, count: Int(funcCount)) {
        TFE_ContextAddFunction(self.cContext, function, self.status)
        checkOk(self.status)
        TF_DeleteFunction(function)
      }

      // Memorize the loaded program by address.
      loadedTFEPrograms.insert(address)
      debugLog("Done loading a new program.")
    }
  }

  /// Load a serialized TensorFlow program in binary proto format, and return
  /// the resulting TF_Graph. If the program has already been loaded, return the
  /// memoized copy.
  /// - Parameters:
  ///   - address: The address of the serialized program in memory.
  ///   - count: The size of the program in bytes.
  func loadGraphInBytes(_ address: UnsafeRawPointer, count: Int) -> CTFGraph {
    // NOTE: Code is intentionally duplicated with the above version of
    // `loadGraphInBytes`, as the above one is expected to go away soon (due to
    // challenges in XLA support). *Please do not refactor for Swiftiness*.
    debugLog("Loading a program.")

    // If the program is already loaded, do nothing.
    var graph: CTFGraph?
    sync { [unowned self] in
      graph = self.loadedTFPrograms[address]
    }
    if let graph = graph {
      debugLog("Already loaded before.")
      return graph
    }

    // Load the program as a TF_Graph
    debugLog("Loading graph functions.")
    graph = TF_NewGraph()
    // TensorFlow loads things through TF_Buffer.  Create one that avoids
    // redundantly copying the program bytes.
    var programBuf = TF_Buffer(data: address, length: count,
      data_deallocator: nil)
    let graphDefOptions = TF_NewImportGraphDefOptions()
    TF_GraphImportGraphDef(graph, &programBuf, graphDefOptions, self.status)
    TF_DeleteImportGraphDefOptions(graphDefOptions)
    checkOk(self.status)

    // Memorize the loaded program by address.
    sync {
      // In TPU mode, the graph will be rewritten before each execution, so do
      // not use the cache in that case.
      if _RuntimeConfig.executionMode != .tpu {
        loadedTFPrograms[address] = graph
      }
    }
    debugLog("Done loading a new program.")
    return graph!
  }
}

private func dumpTensorContent<Scalar : AccelerableByTensorFlow>(
  _ inputTensor: CTensorHandle, _: Scalar.Type
) {
  let array = ShapedArray<Scalar>(cTensorHandle: inputTensor)
  debugLog("Rank is \(array.rank), shape is \(array.shape).")
  debugLog("""
    The content of the \(array.scalars.count) scalars are: \
    \(array.scalars).
    """)
}

/// Used when _RuntimeConfig.usesTFEagerAPI is true.
private class TFEState {
  let status: CTFStatus = TF_NewStatus()

  /// The TFE_Op that the program executes.
  let op: CTFEOp

  init(_ programByteAddress: UnsafeRawPointer,
       programByteCount: Int,
       entryFunctionNameAddress: UnsafePointer<Int8>) {
    let context = _ExecutionContext.global
    // Make sure the program is loaded into the context.
    context.loadProgramInBytes(programByteAddress, count: programByteCount)

    op = context.withMutableCContext { [status] ctx in
      defer { checkOk(status) }
      return TFE_NewOp(ctx, entryFunctionNameAddress, status)
    }
  }

  deinit {
    TFE_DeleteOp(op)
    TF_DeleteStatus(status)
  }
}

extension TFEState {
  func addInput(_ inputTensorHandle: CTensorHandle) {
    TFE_OpAddInput(op, inputTensorHandle, status)
  }
}

/// Used when _RuntimeConfig.usesTFEagerAPI is false.
private class TFState {
  let status: CTFStatus = TF_NewStatus()

  /// The TF_Session to execute the function.
  let cSession: CTFSession
  /// The graph that contains the function to execute. Not owned.
  let graph: CTFGraph
  /// The input tensors.
  var inputTensors: [CTensor?] = []

  init(_ programByteAddress: UnsafeRawPointer,
       programByteCount: Int) {
    // Make sure the program is loaded to the context.
    graph = _ExecutionContext.global.loadGraphInBytes(programByteAddress,
                                                      count: programByteCount)
    let opts = TF_NewSessionOptions()
    if _RuntimeConfig.executionMode == .xla {
      debugLog("Enable XLA execution.")
      TF_EnableXLACompilation(opts, 1)
    }
    cSession = TF_NewSession(graph, opts, status)
    checkOk(status)
    TF_DeleteSessionOptions(opts)
  }

  deinit {
    TF_DeleteSession(cSession, status)
    checkOk(status)
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

  func execute(_ entryFuncName: String,
               _ returnValues: inout [CTensorHandle?]) {
    let funcNode = TF_GraphOperationByName(graph, "tfc_func_" + entryFuncName)
    internalConsistencyCheck(
      funcNode != nil,
      "Cannot find func node name \(entryFuncName)"
    )
    internalConsistencyCheck(
      TF_OperationNumOutputs(funcNode) == returnValues.count
    )

    // Prepare input related parameters for TF_SessionRun().
    var inputNodeSpecs: [TF_Output] = []
    for i in 0..<inputTensors.count {
      let inputNodeName = String("tfc_input_\(i)_\(entryFuncName)")
      let inputNode = TF_GraphOperationByName(graph, inputNodeName)
      internalConsistencyCheck(inputNode != nil,
        "Cannot find input node name \(inputNodeName)")
      inputNodeSpecs.append(TF_Output(oper: inputNode, index: 0))
    }

    // Prepare output related parameters for TF_SessionRun().
    var outputNodeSpecs = (0..<Int32(returnValues.count)).map { i in
      TF_Output(oper: funcNode, index: i)
    }
    var outputTensors: [CTensor?] = Array(repeating: nil,
                                          count: returnValues.count)

    if returnValues.count > 0 {
      debugLog("Calling TF_SessionRun on function \(entryFuncName).")
      if _RuntimeConfig.executionMode == .tpu {
        debugLog("Enable TPU execution.")
        var tpuRewrittenSpecs: [TF_Output] = Array(
          repeating: TF_Output(oper: nil, index: -1),
          count: returnValues.count)
        TF_SetupTPUExecution(cSession,
                              Int32(inputNodeSpecs.count), inputNodeSpecs,
                              Int32(outputNodeSpecs.count), outputNodeSpecs,
                              &tpuRewrittenSpecs, status)
        checkOk(status)
        outputNodeSpecs = tpuRewrittenSpecs
      }
      TF_SessionRun(
        cSession, nil,
        // input related parameters
        inputNodeSpecs, inputTensors, Int32(inputTensors.count),
        // output related parameters
        outputNodeSpecs, &outputTensors, Int32(returnValues.count),
        /*targets*/nil, 0,
        /*run_metadata*/nil, status
      )
      checkOk(status)
      debugLog("Done calling TF_SessionRun.")
    } else {
      // TF_SessionRun() does not support execution that involves no
      // outputs. In this case we assume the TF execution is side-effect free,
      // so skipping is a valid optimization without changing behavior.
      //
      // This case usually only occurs in compiler-only unit tests, where the
      //generated TF program does not produce any outputs to be consumed by
      //Swift host code.
      debugLog("Skipping calling TF_SessionRun since there are no outputs.")
    }

    // Delete input tensors.
    for inputTensor in inputTensors {
      TF_DeleteTensor(inputTensor)
    }

    // Synthesize TFE tensor handles to work with the existing Swift TF
    // library code.
    for i in 0..<returnValues.count {
      returnValues[i] = TFE_NewTensorHandle(outputTensors[i], status)
      checkOk(status)
      TF_DeleteTensor(outputTensors[i])
    }
  }
}

//===----------------------------------------------------------------------===//
// - MARK: Tensor computation
//===----------------------------------------------------------------------===//

/// Tensor program.
///
/// - Note: The call sequence for the APIs below must be one of the two:
///    init -> terminate()
///    init -> finish()
///   The finish/terminate APIs may only be called once.
public final class _TensorComputation {
  /// The status for checking TensorFlow errors.
  let status: CTFStatus = TF_NewStatus()

  /// The function to execute in this instance.
  let entryFuncName: String

  /// The tensor handles returned by the tensor program.
  /// TODO(hongm): Retire returnValues when eager based runtime is removed.
  var returnValues: [CTensorHandle?]

  // NOTE: the following properties are intentionally not implemented as an enum
  // due to high churn, *please do not refactor for Swiftiness*.
  private var stateTFE: TFEState?
  private var stateTF: TFState?

  /// The thread to run tensor computation in. The global config flag
  /// '_RuntimeConfig.usesSynchronousExecution' decides whether tensor
  /// computation should be synchronous: if true, this property will be nil.
  ///
  /// - TODO(hongm): For pthread portability on Darwin and other OSes, see
  ///   swift/stdlib/private/SwiftPrivatePthreadExtras/SwiftPrivatePthreadExtras.swift
  ///   https://github.com/ketzusaka/Strand/blob/master/Sources/Strand.swift
  ///   Also assess Windows portability (where pthread_create does not exist).
#if os(Linux) || os(FreeBSD)
  private var pthread: pthread_t? =
    _RuntimeConfig.usesSynchronousExecution ? nil : pthread_t()
#else
  private var pthread: Int? =
    _RuntimeConfig.usesSynchronousExecution ? nil : 1
#endif

  /// Load the TF program from a binary TF FunctionDef proto given by
  /// 'programByteAddress' and 'programByteCount', and start the computation.
  ///
  /// - Parameters:
  ///   - programByteAddress: The address of the raw program.
  ///   - programByteCount: The number of bytes in the program.
  ///   - tensorArgumentAddress: The address to the buffer containing tensor
  ///     arguments as CTensorHandle.
  ///   - tensorArgumentCount: The number of tensor arguments to pass in.
  ///
  /// - TODO(clattner): resultCount should go away when the runtime is
  ///   implemented with an async design.
  @_versioned
  init(programByteAddress: UnsafeRawPointer,
       programByteCount: Int,
       entryFunctionNameAddress: UnsafePointer<Int8>,
       tensorArgumentAddress: UnsafePointer<CTensorHandle>,
       tensorArgumentCount: Int,
       resultCount: Int) {
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
    entryFuncName = String(cString: entryFunctionNameAddress)
    debugLog("""
      Creating a new op with func name \
      \(String(cString: entryFunctionNameAddress)).
      """);
    if _RuntimeConfig.usesTFEagerAPI {
      self.stateTFE = TFEState(programByteAddress,
        programByteCount: programByteCount,
        entryFunctionNameAddress: entryFunctionNameAddress)
      debugLog("Done initializing TFE-specific state.")
    } else {
      self.stateTF = TFState(programByteAddress,
        programByteCount: programByteCount)
      debugLog("Done initializing TF-specific state.")
    }

    if _RuntimeConfig.executionMode == .gpu {
      guard let gpuDeviceName = context.gpuDeviceName else {
        fatalError("""
          The availability of a GPU device should have been confirmed \
          at this point.
          """)
      }
      if let stateTFE = stateTFE {
        internalConsistencyCheck(_RuntimeConfig.usesTFEagerAPI)
        debugLog("Setting op device to \(gpuDeviceName).")
        TFE_OpSetDevice(stateTFE.op, gpuDeviceName, status)
        checkOk(status)
      }
      // For C API no explicit placement is needed.
    }

    debugLog("Populating the op's input list.")
    for (i, inputTensorHandle) in inputTensorHandles.enumerated() {
      if _RuntimeConfig.printsDebugLog {
        let dType: TF_DataType = TFE_TensorHandleDataType(inputTensorHandle)
        debugLog("Input tensor \(i) has TF data type \(dType).")
        switch(dType) {
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

    // If it's asynchronous, we start a pthread that calls execute().
    // NOTE: Currently, asynchronous execution is only supported on Linux.
    if pthread != nil {
#if os(Linux) || os(FreeBSD)
      // The function to launch in the parallel thread.
      func threadBody(
        _ arg: UnsafeMutableRawPointer?
      ) -> UnsafeMutableRawPointer? {
        // Set the cancelability of the detached thread.
        pthread_setcanceltype(Int32(PTHREAD_CANCEL_DEFERRED), nil)
        // Execute the tensor computation.
        let computation: _TensorComputation =
          Unmanaged.fromOpaque(arg!).takeRetainedValue()
        computation.execute()
        checkOk(computation.status)
        return nil
      }
      let creationStatus = pthread_create(
        &self.pthread!, nil, threadBody,
        Unmanaged.passRetained(self).toOpaque()
      )
      // TODO(hongm): do error handling.
      internalConsistencyCheck(creationStatus == 0)
#else
      fatalError("Asynchronous execution not supported on this host yet")
#endif
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
  /// Execute the computation using TensorFlow Eager.
  /// NOTE: This is to be called by the initializer. The computation gets
  /// executed on initialization, thus this method will not be exposed to users.
  private func execute() {
    debugLog("Executing TF function \(entryFuncName).")
    var returnValueCount = Int32.max
    if let stateTFE = stateTFE {
      internalConsistencyCheck(_RuntimeConfig.usesTFEagerAPI)
      TFE_Execute(stateTFE.op, &returnValues, &returnValueCount, status)
      debugLog("""
        returnValues.count=\(returnValues.count), \
        returnValueCount=\(returnValueCount).
        """)
      assert(returnValueCount == returnValues.count)
      debugLog("Done execution with eager.")
      return
    }

    // Non-eager based execution.
    internalConsistencyCheck(!_RuntimeConfig.usesTFEagerAPI)
    guard let stateTF = stateTF else {
      fatalError("""
        stateTF must be defined when _RuntimeConfig.usesTFEagerAPI == false.
        """)
    }
    stateTF.execute(entryFuncName, &returnValues)
    debugLog("Done execution with non-eager.")
  }
}

public extension _TensorComputation {
  /// Terminate the computation, and clean up the state.
  func terminate() {
#if os(Linux) || os(FreeBSD)
    if let pthread = pthread {
      // TODO(hongm): Assess TF's thread cancel support.
      let cancelStatus = pthread_cancel(pthread)
      internalConsistencyCheck(cancelStatus == 0)
      self.pthread = nil
    }
#endif
  }

  /// Wait for completion the computation as given by 'program', and returns
  /// output handles, whose underlying tensors may live on CPU or GPU.
  func finish() -> [CTensorHandle] {
    debugLog("Calling _TensorComputation.finish().")
#if os(Linux) || os(FreeBSD)
    if let pthread = pthread {
      debugLog("Waiting for thread to join.")
      let joinStatus = pthread_join(pthread, nil)
      internalConsistencyCheck(joinStatus == 0)
      self.pthread = nil
    }
#endif
    debugLog("Done executing TF graph.")

    // Now that all the elements have been filled in, remove a level of
    // optional.
    return returnValues.map { $0! }
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

/// Load the TF computation from a binary TF FunctionDef proto given by 'bytes'
/// and 'size', start the computation, and return a _TensorComputation object as
/// a unique identifier for that computation.
///
/// - Parameters:
///   - programByteAddress: The address of the raw program.
///   - programByteCount: The number of bytes in the program.
///   - tensorArgumentAddress: The address to the buffer containing tensor
///     arguments as CTensorHandle.
///   - tensorArgumentCount: The number of tensor arguments to pass in.
@_inlineable
@_silgen_name("_swift_tfc_StartTensorComputation")
public func _TFCStartTensorComputation(
  _ programByteAddress: UnsafeRawPointer,
  _ programByteCount: Int,
  _ entryFunctionNameAddress: UnsafePointer<Int8>,
  _ tensorArgumentAddress: UnsafePointer<CTensorHandle>,
  _ tensorArgumentCount: Int,
  // TODO(clattner): resultCount should go away when the runtime is implemented
  // with an async design.
  _ resultCount: Int
) -> _TensorComputation {

  debugLog("""
    _TFCStartTensorComputation() is called with \(programByteCount) \
    program bytes, \(tensorArgumentCount) input tensors \
    \(String(cString:entryFunctionNameAddress)) as the func name, and \
    \(resultCount) output tensors.
    """)

  return _TensorComputation(programByteAddress: programByteAddress,
                            programByteCount: programByteCount,
                            entryFunctionNameAddress: entryFunctionNameAddress,
                            tensorArgumentAddress: tensorArgumentAddress,
                            tensorArgumentCount: tensorArgumentCount,
                            resultCount: resultCount)
}

/// Wait for completion the computation as given by 'program', and returns
/// results.
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

/// Terminate the computation as given by 'program', and clean up the state.
///
/// - Parameters:
///   - program: The tensor program to terminate.
/// - Note: If the execution was synchronous, then this function does nothing.
@_inlineable
@_silgen_name("_swift_tfc_TerminateTensorComputation")
public func _TFCTerminateTensorComputation(_ computation: _TensorComputation) {
  computation.terminate()
}

/// Create a scalar CTensorHandle value for the given data type.
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
