// RUN: %target-run-simple-swift( -plugin-path %swift-plugin-dir -target %target-swift-5.1-abi-triple -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: reflection

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

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
func synchronous_bind() {

  func synchronous() {
    printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (1111)

    TL.$number.withValue(2222) {
      printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (2222)
    }

    printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (1111)
  }

  TL.$number.withValue(1111) {
    synchronous()
  }
}

@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() {
    synchronous_bind()
  }
}
