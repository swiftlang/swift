// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency -parse-as-library %import-libdispatch) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

class StringLike: CustomStringConvertible {
  let value: String
  init(_ value: String) {
    self.value = value
  }

  var description: String { value }
}

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

extension TaskLocalValues {

  struct StringKey: TaskLocalKey {
    static var defaultValue: String { .init("<undefined>") }
    static var inherit: TaskLocalInheritance { .never }
  }
  var string: StringKey { .init() }

}

// ==== ------------------------------------------------------------------------

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

func test_async_group() async {
  // CHECK: test_async_group
  print(#function)

  printTaskLocal(\.string) // CHECK: StringKey: <undefined> {{.*}}
  await Task.withLocal(\.string, boundTo: "top") {
    printTaskLocal(\.string) // CHECK: StringKey: top {{.*}}

    try! await Task.withGroup(resultType: Void.self) { group -> Void? in
      printTaskLocal(\.string) // CHECK: StringKey: top {{.*}}

      await group.add {
        printTaskLocal(\.string) // CHECK: StringKey: <undefined> {{.*}}
      }

      return try! await group.next()
    }
  }
}

@main struct Main {
  static func main() async {
    await test_async_let()
    await test_async_group()
  }
}
