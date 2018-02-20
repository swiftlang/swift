import TensorFlow
import StdlibUnittest

extension TestSuite {
  public func testCPUAndGPU(_ name: String, _ body: @escaping () -> Void) {
    testCPU(name, body)
    testGPU(name, body)
  }
  // For now, each test will run in both eager and non-eager modes.  We expect
  // to remove eager mode support due to challenges in full XLA support there.
  public func testCPU(_ name: String, _ body: @escaping () -> Void) {
    test(name + "_CPU_eager") {
      _RuntimeConfig.usesTFEagerAPI = true
      _RuntimeConfig.runsOnGPU = false
      _RuntimeConfig.printsDebugLog = false
      body()
    }
    test(name + "_CPU") {
      _RuntimeConfig.usesTFEagerAPI = false
      _RuntimeConfig.runsOnGPU = false
      _RuntimeConfig.printsDebugLog = false
      body()
    }
  }
  public func testGPU(_ name: String, _ body: @escaping () -> Void) {
#if CUDA
    // Run eager tests before non-eager ones, so that _ExecutionContext.init()
    // will call TFE_ContextOptionsSetDevicePlacementPolicy() properly.
    test(name + "_GPU_eager") {
      _RuntimeConfig.usesTFEagerAPI = true
      _RuntimeConfig.runsOnGPU = true
      _RuntimeConfig.printsDebugLog = false
      body()
    }
    test(name + "_GPU") {
      _RuntimeConfig.usesTFEagerAPI = false
      _RuntimeConfig.runsOnGPU = true
      _RuntimeConfig.printsDebugLog = false
      body()
    }
#endif
  }
}

/// Loop testing on GPU is only supported via XLA, which in turn only
/// supports TF C API. So for TF eager C API + GPU execution, the test is
/// skipped.
public func shouldDoLoopTest() -> Bool {
  if _RuntimeConfig.usesTFEagerAPI &&
     _ExecutionContext.global.gpuDeviceName != nil {
    print("Loop tests are skipped in Eager + GPU mode.")
    return false
  }
  _RuntimeConfig.usesXLA = true
  return true
}
