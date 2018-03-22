import CTensorFlow
import CTensorFlowTestClusterAPI
import TensorFlow
import StdlibUnittest

// Checks if an optional OpaquePointer representing a CTFStatus has value TF_OK.
public func checkOk(_ s: OpaquePointer?, file: StaticString = #file,
                    line: UInt = #line) {
  assert(TF_GetCode(s) == TF_OK,
         String(cString: TF_Message(s)),
         file: file, line: line)
}

/// Determines if two floating point numbers are very nearly equal.
public func expectNearlyEqual<T : FloatingPoint & ExpressibleByFloatLiteral>(
  _ lhs: T, _ rhs: T, byError error: T = 0.000001
) {
  expectLT(abs(lhs - rhs), error)
}

/// Determines if two collections of floating point numbers are very nearly
/// equal.
public func expectPointwiseNearlyEqual<T, C1, C2>(
  _ lhs: C1, _ rhs: C2, byError error: T = 0.000001
) where T : FloatingPoint & ExpressibleByFloatLiteral,
  C1 : Collection, C2 : Collection, C1.Element == T, C1.Element == C2.Element {
  precondition(lhs.count == rhs.count, "Scalar count mismatch.")
  for (l, r) in zip(lhs, rhs) {
    expectNearlyEqual(l, r, byError: error)
  }
}

extension TestSuite {
  // Use the three macros CPU, CUDA and TPU to differentiate the 3 associated
  // backends.
  // Notes:
  // 1. When Cuda is enabled, we still run testCPU(). This might change in the
  // future, as the runtime based on TF C API will place nodes on GPU when GPU
  // is available.
  // 2. TPU is mutually exclusive with {CPU, CUDA} (requiring different
  // hardware).
  public func testAllBackends(_ name: String, _ body: @escaping () -> Void) {
#if CPU
    testCPU(name, body)
#endif
#if CUDA && !TPU
    testGPU(name, body)
#endif // CUDA && !TPU
#if TPU && !CUDA
    // If CUDA is also defined, don't run the TPU tests.
    testTPU(name, body)
#endif // TPU && !CUDA
  }
  // For now, each CPU test will run in both eager and non-eager modes.  We
  // expect to remove eager mode support due to challenges in full XLA support
  // there.
  public func testCPU(_ name: String, _ body: @escaping () -> Void) {
#if CPU
#if !CUDA
    // Do not run eager tests under GPU, to save resource and run-time.
    test(name + "_CPU_eager") {
      _RuntimeConfig.usesTFEagerAPI = true
      _RuntimeConfig.executionMode = .cpu
      _RuntimeConfig.printsDebugLog = false
      body()
    }
#endif // CUDA
    test(name + "_CPU") {
      _RuntimeConfig.usesTFEagerAPI = false
      _RuntimeConfig.executionMode = .cpu
      _RuntimeConfig.printsDebugLog = false
      body()
    }
#endif // CPU
  }
  public func testGPU(_ name: String, _ body: @escaping () -> Void) {
#if CUDA && !TPU
    test(name + "_GPU") {
      // To save resources and runtime, do not run GPU tests under eager.
      _RuntimeConfig.usesTFEagerAPI = false
      _RuntimeConfig.executionMode = .gpu
      _RuntimeConfig.printsDebugLog = false
      body()
    }
#endif // CUDA && !TPU
  }
  public func testTPU(_ name: String, _ body: @escaping () -> Void) {
#if TPU && !CUDA
    test(name + "_TPU") {
      _RuntimeConfig.usesTFEagerAPI = false
      _RuntimeConfig.executionMode = .tpu
      _RuntimeConfig.printsDebugLog = false
      body()
    }
#endif // TPU && !CUDA
  }
}

/// Loops can run on these backends:
/// 1. TF interpreter with CPU (not covered in the testing here, for
/// simplicity)
/// 2. XLA (CPU or GPU)
/// 3. TPU
public func shouldDoLoopTest() -> Bool {
  if _RuntimeConfig.usesTFEagerAPI &&
     _ExecutionContext.global.gpuDeviceName != nil {
    print("Loop tests are skipped in Eager + GPU mode.")
    return false
  }
  if !_RuntimeConfig.executionMode.isTPU {
    _RuntimeConfig.executionMode = .xla
  }
  return true
}

public func runAllTestsWithRemoteSession() {
  let status = TF_NewStatus()
  let cluster_handle = TF_NewClusterHandle()
  let remote_server_address = TF_StartTensorFlowProcessCluster(
    /*num_processes*/ 2, cluster_handle, status)
  checkOk(status)
  _RuntimeConfig.session = .remote(
    grpcAddress: "grpc://" + String(cString: remote_server_address!))

  runAllTests()

  TF_StopTensorFlowProcessCluster(cluster_handle)
}
