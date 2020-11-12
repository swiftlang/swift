// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency  %import-libdispatch) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

import Dispatch
import Foundation

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#endif

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
) async throws where Key: TaskLocalKey {
  let value = await Task.local(key)
  print("\(Key.self): \(value) at \(file):\(line)")
  if let expected = expected {
    assert("\(expected)" == "\(value)",
      "Expected [\(expected)] but found: \(value), at \(file):\(line)")
  }
}

// ==== ------------------------------------------------------------------------


func test2() async {
  try! await printTaskLocal(\.number)
}

func async_let_nested() async {
  _ = try! await printTaskLocal(\.number) // CHECK: NumberKey: 0 {{.*}}
  async let x1 = Task.withLocal(\.number, boundTo: 2) {
    async let x2 = printTaskLocal(\.number) // CHECK: NumberKey: 2 {{.*}}

    func test() async {
      try! await printTaskLocal(\.number) // CHECK: NumberKey: 2 {{.*}}
      async let x31 = test2() // CHECK: NumberKey: 2 {{.*}}
      try! await x31
    }
    async let x3 = test()

    try! await x2
    try! await x3
  }

  _ = try! await x1
  try! await printTaskLocal(\.number) // CHECK: NumberKey: 0 {{.*}}
}

runAsyncAndBlock(async_let_nested)
