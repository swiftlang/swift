// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency -parse-as-library %import-libdispatch) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// UNSUPPORTED: use_os_stdlib

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
class StringLike: CustomStringConvertible {
  let value: String
  init(_ value: String) {
    self.value = value
  }

  var description: String { value }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func printTaskLocal<Key>(
  _ key: KeyPath<TaskLocalValues, Key>,
  _ expected: Key.Value? = nil,
  file: String = #file, line: UInt = #line
) where Key: TaskLocalKey {
  let value = Task.local(key)
  print("\(Key.self): \(value) at \(file):\(line)")
  if let expected = expected {
    assert("\(expected)" == "\(value)",
      "Expected [\(expected)] but found: \(value), at \(file):\(line)")
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension TaskLocalValues {

  struct StringKey: TaskLocalKey {
    static var defaultValue: String { .init("<undefined>") }
    static var inherit: TaskLocalInheritance { .never }
  }
  var string: StringKey { .init() }

}

// ==== ------------------------------------------------------------------------

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func test_async_let() async {
  print(#function) // CHECK: test_async_let

  printTaskLocal(\.string) // CHECK: StringKey: <undefined> {{.*}}
  await Task.withLocal(\.string, boundTo: "top") {
    printTaskLocal(\.string) // CHECK: StringKey: top {{.*}}

    async let child: () = printTaskLocal(\.string) // CHECK: StringKey: <undefined> {{.*}}
    await child

    printTaskLocal(\.string) // CHECK: StringKey: top {{.*}}
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func test_async_group() async {
  // CHECK: test_async_group
  print(#function)

  printTaskLocal(\.string) // CHECK: StringKey: <undefined> {{.*}}
  await Task.withLocal(\.string, boundTo: "top") {
    printTaskLocal(\.string) // CHECK: StringKey: top {{.*}}

    await withTaskGroup(of: Int.self, returning: Void.self) { group in
      printTaskLocal(\.string) // CHECK: StringKey: top {{.*}}

      group.spawn {
        printTaskLocal(\.string) // CHECK: StringKey: <undefined> {{.*}}
        return 0
      }

      _ = await group.next()
    }
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@main struct Main {
  static func main() async {
    await test_async_let()
    await test_async_group()
  }
}
