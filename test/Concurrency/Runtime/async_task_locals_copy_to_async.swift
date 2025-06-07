// REQUIRES: rdar80824152
// RUN: %target-run-simple-swift( -plugin-path %swift-plugin-dir -target %target-swift-5.1-abi-triple -parse-as-library %import-libdispatch) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

@available(SwiftStdlib 5.1, *)
enum TL {
  @TaskLocal
  static var number: Int = 0
  @TaskLocal
  static var other: Int = 0
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
func copyTo_async() async {
  await TL.$number.withValue(1111) {
    printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (1111)

    await TL.$number.withValue(2222) {
      await TL.$other.withValue(9999) {
        printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (2222)
        printTaskLocal(TL.$other) // CHECK: TaskLocal<Int>(defaultValue: 0) (9999)
        let handle = Task {
          printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (2222)
          printTaskLocal(TL.$other) // CHECK: TaskLocal<Int>(defaultValue: 0) (9999)
          TL.$number.withValue(3333) {
            printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (3333)
            printTaskLocal(TL.$other) // CHECK: TaskLocal<Int>(defaultValue: 0) (9999)
          }
        }

        _ = await handle.value
      }
    }
  }
}

@available(SwiftStdlib 5.1, *)
func copyTo_async_noWait() async {
  print(#function)
  TL.$number.withValue(1111) {
    TL.$number.withValue(2222) {
      TL.$other.withValue(9999) {
        Task {
          printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (2222)
          printTaskLocal(TL.$other) // CHECK: TaskLocal<Int>(defaultValue: 0) (9999)
          TL.$number.withValue(3333) {
            printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (3333)
            printTaskLocal(TL.$other) // CHECK: TaskLocal<Int>(defaultValue: 0) (9999)
          }
        }
      }
    }
  }

  let second = UInt64(100_000_000) // ns
  await Task.sleep(2 * second)
}

@available(SwiftStdlib 5.1, *)
class CustomClass {
  @TaskLocal
  static var current: CustomClass?

  init() {
    print("init \(ObjectIdentifier(self))")
  }

  deinit {
    print("deinit \(ObjectIdentifier(self))")
  }
}

@available(SwiftStdlib 5.1, *)
func test_unstructured_retains() async {
  let instance = CustomClass()
  CustomClass.$current.withValue(instance) {
    print("BEFORE send: \(String(reflecting: CustomClass.current))")
    // don't await on the un-structured tasks on purpose, we want to see that the tasks
    // themselves keep the object alive even if we don't hold onto them
    Task {
      print("in async task: \(String(reflecting: CustomClass.current))")
    }
    Task {
      print("in async task: \(String(reflecting: CustomClass.current))")
    }
    print("AFTER send: \(String(reflecting: CustomClass.current))")
  }

  // CHECK: init
  // CHECK: BEFORE send: Optional(main.CustomClass)
  // CHECK: in async task: Optional(main.CustomClass)
  // CHECK: in async task: Optional(main.CustomClass)
  // the deinit MUST NOT happen before the async tasks runs
  // CHECK: deinit
  await Task.sleep(2 * 1_000_000_000)
}

@available(SwiftStdlib 5.1, *)
func test_unstructured_noValues() async {
  await Task {
    // no values to copy
  }.value
}

@available(SwiftStdlib 5.1, *)
func downloadImage(from url: String) async throws -> String {
  await Task.sleep(10_000)
  return ""
}

@available(SwiftStdlib 5.1, *)
func test_unstructured_noValues_childTasks() async {
  @Sendable func work() async throws {
    let handle = Task {
      try await downloadImage(from: "")
    }
  }

  // these child tasks have a parent pointer in their task local storage.
  // we must not copy it when performing the copyTo for a new unstructured task.
  async let one = work()
  async let two = work()
  async let three = work()

  try! await one
  try! await two
  try! await three

}

@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async {
    await copyTo_async()
    await copyTo_async_noWait()
    await test_unstructured_retains()
    await test_unstructured_noValues()
    await test_unstructured_noValues_childTasks()
  }
}
