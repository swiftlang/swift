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

// ==== ------------------------------------------------------------------------


func groups() async {
  // no value
  try! await Task.withGroup(resultType: Int.self) { group in
    printTaskLocal(\.number) // CHECK: NumberKey: 0 {{.*}}
  }

  // no value in parent, value in child
  let x1: Int = try! await Task.withGroup(resultType: Int.self) { group in
    await group.add {
      printTaskLocal(\.number) // CHECK: NumberKey: 0 {{.*}}
      // inside the child task, set a value
      await Task.withLocal(\.number, boundTo: 1) {
        printTaskLocal(\.number) // CHECK: NumberKey: 1 {{.*}}
      }
      printTaskLocal(\.number) // CHECK: NumberKey: 0 {{.*}}
      return Task.local(\.number) // 0
    }

    return try! await group.next()!
  }
  assert(x1 == 0)

  // value in parent and in groups
  await Task.withLocal(\.number, boundTo: 2) {
    printTaskLocal(\.number) // CHECK: NumberKey: 2 {{.*}}

    let x2: Int = try! await Task.withGroup(resultType: Int.self) { group in
      printTaskLocal(\.number) // CHECK: NumberKey: 2 {{.*}}
      await group.add {
        printTaskLocal(\.number) // CHECK: NumberKey: 2 {{.*}}

        async let childInsideGroupChild: () = printTaskLocal(\.number)
        await childInsideGroupChild // CHECK: NumberKey: 2 {{.*}}

        return Task.local(\.number)
      }
      printTaskLocal(\.number) // CHECK: NumberKey: 2 {{.*}}

      return try! await group.next()!
    }

    assert(x2 == 2)
  }
}

@main struct Main {
  static func main() async {
    await groups()
  }
}
