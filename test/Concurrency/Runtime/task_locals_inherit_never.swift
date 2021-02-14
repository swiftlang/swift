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
) async where Key: TaskLocalKey {
  let value = await Task.local(key)
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
  // CHECK: StringKey: <undefined> {{.*}}
  await printTaskLocal(\.string)
  await Task.withLocal(\.string, boundTo: "top") {
    // CHECK: StringKey: top {{.*}}
    await printTaskLocal(\.string)

    // CHECK: StringKey: <undefined> {{.*}}
    async let child: () = printTaskLocal(\.string)
    await child

    // CHECK: StringKey: top {{.*}}
    await printTaskLocal(\.string)
  }
}

func pending_async_group() async {
  print("SKIPPED: \(#function)") // FIXME: unlock once https://github.com/apple/swift/pull/35874 is merged
  return

  // COM: CHECK: test_async_group
  print(#function)

  // COM: CHECK: StringKey: <undefined> {{.*}}
  await printTaskLocal(\.string)
  await Task.withLocal(\.string, boundTo: "top") {
    // COM: CHECK: StringKey: top {{.*}}
    await printTaskLocal(\.string)

    try! await Task.withGroup(resultType: String.self) { group -> Void in
      // COM: CHECK: StringKey: top {{.*}}
      await printTaskLocal(\.string)
    }
  }
}

@main struct Main {
  static func main() async {
    await test_async_let()
    await pending_async_group()
  }
}
