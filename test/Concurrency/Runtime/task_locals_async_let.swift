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

extension TaskLocalValues {
  struct NumberKey: TaskLocalKey {
    static var defaultValue: Int { 0 }
  }
  var number: NumberKey { .init() }
}

@discardableResult
func printTaskLocal<Key>(
  _ key: KeyPath<TaskLocalValues, Key>,
  _ expected: Key.Value? = nil,
  file: String = #file, line: UInt = #line
) async throws -> Key.Value? where Key: TaskLocalKey {
  let value = await Task.local(key)
  print("\(Key.self): \(value) at \(file):\(line)")
  if let expected = expected {
    assert("\(expected)" == "\(value)",
      "Expected [\(expected)] but found: \(value), at \(file):\(line)")
  }
  return expected
}

// ==== ------------------------------------------------------------------------

func async_let_nested() async {
  _ = try! await printTaskLocal(\.number) // COM: NumberKey: 0 {{.*}}
  async let x1: () = Task.withLocal(\.number, boundTo: 2) {
    async let x2 = printTaskLocal(\.number) // COM: NumberKey: 2 {{.*}}

    @concurrent
    func test() async {
      try! await printTaskLocal(\.number) // COM: NumberKey: 2 {{.*}}
      async let x31 = printTaskLocal(\.number) // COM: NumberKey: 2 {{.*}}
      _ = try! await x31
    }
    async let x3: () = test()

    _ = try! await x2
    await x3
  }

  _ = await x1
  try! await printTaskLocal(\.number) // COM: NumberKey: 0 {{.*}}
}

func async_let_nested_skip_optimization() async {
  async let x1: Int? = Task.withLocal(\.number, boundTo: 2) {
    async let x2: Int? = { () async -> Int? in
      async let x3: Int? = { () async -> Int? in
        async let x4: Int? = { () async -> Int? in
          async let x5: Int? = { () async -> Int? in
            async let xx = printTaskLocal(\.number) // CHECK: NumberKey: 2 {{.*}}
            return try! await xx
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

@main struct Main {
  static func main() async {
    await async_let_nested()
    await async_let_nested_skip_optimization()
  }
}
