import TensorFlow
import StdlibUnittest

extension TestSuite {
  public func testCPUAndGPU(_ name: String, _ body: @escaping () -> Void) {
    testCPU(name, body)
    testGPU(name, body)
  }
  public func testCPU(_ name: String, _ body: @escaping () -> Void) {
    test(name + "_CPU") {
      _RuntimeConfig.runsOnGPU = false
      body()
    }
  }
  public func testGPU(_ name: String, _ body: @escaping () -> Void) {
#if CUDA
    test(name + "_GPU") {
      _RuntimeConfig.runsOnGPU = true
      body()
    }
#endif
  }
}
