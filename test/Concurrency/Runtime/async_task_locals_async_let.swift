// RUN: %target-run-simple-swift( -plugin-path %swift-plugin-dir -target %target-swift-5.1-abi-triple -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: reflection

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

// rdar://105496007
// UNSUPPORTED: CPU=arm64e

@available(SwiftStdlib 5.1, *)
enum TL {
  @TaskLocal
  static var number: Int = 0
}

@available(SwiftStdlib 5.1, *)
@discardableResult
func printTaskLocal<V>(
    _ key: TaskLocal<V>,
    _ expected: V? = nil,
    file: String = #file, line: UInt = #line
) -> V? {
  let value = key.get()
  print("\(key) (\(value)) at \(file):\(line)")
  if let expected = expected {
    assert("\(expected)" == "\(value)",
        "Expected [\(expected)] but found: \(value), at \(file):\(line)")
  }
  return expected
}

// ==== ------------------------------------------------------------------------

@available(SwiftStdlib 5.1, *)
func async_let_nested() async {
  printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (0)
  async let x1: () = TL.$number.withValue(2) {
    async let x2 = printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (2)

    @Sendable
    func test() async {
      printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (2)
      async let x31 = printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (2)
      _ = await x31
    }
    async let x3: () = test()

    _ = await x2
    await x3
  }

  _ = await x1
  printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (0)
}

@available(SwiftStdlib 5.1, *)
func async_let_nested_skip_optimization() async {
  async let x1: Int? = TL.$number.withValue(2) {
    async let x2: Int? = { () async -> Int? in
      async let x3: Int? = { () async -> Int? in
        async let x4: Int? = { () async -> Int? in
          async let x5: Int? = { () async -> Int? in
            assert(TL.$number.get() == 2)
            async let xx = printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (2)
            return await xx
          }()
          return await x5
        }()
        return await x4
      }()
      return await x3
    }()
    return await x2
  }

  _ = await x1
}

@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async {
    await async_let_nested()
    await async_let_nested_skip_optimization()
  }
}
