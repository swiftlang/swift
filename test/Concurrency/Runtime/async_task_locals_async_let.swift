// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency -parse-as-library %import-libdispatch) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// UNSUPPORTED: use_os_stdlib

class StringLike: CustomStringConvertible {
  let value: String
  init(_ value: String) {
    self.value = value
  }

  var description: String { value }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension TaskLocalValues {
  struct NumberKey: TaskLocalKey {
    static var defaultValue: Int { 0 }
  }
  var number: NumberKey { .init() }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@discardableResult
func printTaskLocal<Key>(
  _ key: KeyPath<TaskLocalValues, Key>,
  _ expected: Key.Value? = nil,
  file: String = #file, line: UInt = #line
) -> Key.Value? where Key: TaskLocalKey {
  let value = Task.local(key)
  print("\(Key.self): \(value) at \(file):\(line)")
  if let expected = expected {
    assert("\(expected)" == "\(value)",
      "Expected [\(expected)] but found: \(value), at \(file):\(line)")
  }
  return expected
}

// ==== ------------------------------------------------------------------------

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func async_let_nested() async {
  _ = printTaskLocal(\.number) // CHECK: NumberKey: 0 {{.*}}
  async let x1: () = Task.withLocal(\.number, boundTo: 2) {
    async let x2 = printTaskLocal(\.number) // CHECK: NumberKey: 2 {{.*}}

    @Sendable
    func test() async {
      printTaskLocal(\.number) // CHECK: NumberKey: 2 {{.*}}
      async let x31 = printTaskLocal(\.number) // CHECK: NumberKey: 2 {{.*}}
      _ = await x31
    }
    async let x3: () = test()

    _ = await x2
    await x3
  }

  _ = await x1
  printTaskLocal(\.number) // CHECK: NumberKey: 0 {{.*}}
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func async_let_nested_skip_optimization() async {
  async let x1: Int? = Task.withLocal(\.number, boundTo: 2) {
    async let x2: Int? = { () async -> Int? in
      async let x3: Int? = { () async -> Int? in
        async let x4: Int? = { () async -> Int? in
          async let x5: Int? = { () async -> Int? in
            assert(Task.local(\.number) == 2)
            async let xx = printTaskLocal(\.number) // CHECK: NumberKey: 2 {{.*}}
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

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@main struct Main {
  static func main() async {
    await async_let_nested()
    await async_let_nested_skip_optimization()
  }
}
