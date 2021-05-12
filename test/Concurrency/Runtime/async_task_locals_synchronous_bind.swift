// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency -parse-as-library %import-libdispatch) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

@available(SwiftStdlib 5.5, *)
enum TL {
  @TaskLocal
  static var number: Int = 0
}

@available(SwiftStdlib 5.5, *)
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

@available(SwiftStdlib 5.5, *)
func synchronous_bind() async {

  func synchronous() {
    printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (1111)

    TL.$number.withValue(2222) {
      printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (2222)
    }

    printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (1111)
  }

  await TL.$number.withValue(1111) {
    synchronous()
  }
}

@available(SwiftStdlib 5.5, *)
@main struct Main {
  static func main() async {
    await synchronous_bind()
  }
}
