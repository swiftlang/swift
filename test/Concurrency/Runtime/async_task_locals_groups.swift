// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency -parse-as-library %import-libdispatch) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
enum TL {
  @TaskLocal
  static var number = 0
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
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

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func groups() async {
  // no value
  _ = await withTaskGroup(of: Int.self) { group in
    printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (0)
  }

  // no value in parent, value in child
  let x1: Int = await withTaskGroup(of: Int.self) { group in
    group.spawn {
      printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (0)
      // inside the child task, set a value
      _ = await TL.$number.withValue(1) {
        printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (1)
      }
      printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (0)
      return TL.$number.get() // 0
    }

    return await group.next()!
  }
  assert(x1 == 0)

  // value in parent and in groups
  await TL.$number.withValue(2) {
    printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (2)

    let x2: Int = await withTaskGroup(of: Int.self) { group in
      printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (2)
      group.spawn {
        printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (2)

        async let childInsideGroupChild = printTaskLocal(TL.$number)
        _ = await childInsideGroupChild // CHECK: TaskLocal<Int>(defaultValue: 0) (2)

        return TL.$number.get()
      }
      printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (2)

      return await group.next()!
    }

    assert(x2 == 2)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@main struct Main {
  static func main() async {
    await groups()
  }
}
