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

  struct StringKey: TaskLocalKey {
    static var defaultValue: String { .init("<undefined>") }
  }
  var string: StringKey { .init() }

  struct NumberKey: TaskLocalKey {
    static var defaultValue: Int { 0 }
  }
  var number: NumberKey { .init() }

  struct NeverKey: TaskLocalKey {
    static var defaultValue: StringLike { .init("<never>") }
  }
  var never: NeverKey { .init() }

  struct ClazzKey: TaskLocalKey {
    static var defaultValue: ClassTaskLocal? { nil }
  }
  var clazz: ClazzKey { .init() }

}

final class ClassTaskLocal {
  init() {
    print("clazz init \(ObjectIdentifier(self))")
  }

  deinit {
    print("clazz deinit \(ObjectIdentifier(self))")
  }
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

func simple() async {
  try! await printTaskLocal(\.number) // CHECK: NumberKey: 0 {{.*}}
  await Task.withLocal(\.number, boundTo: 1) {
    try! await printTaskLocal(\.number) // CHECK-NEXT: NumberKey: 1 {{.*}}
  }
}

func simple_deinit() async {
  await Task.withLocal(\.clazz, boundTo: ClassTaskLocal()) {
    // CHECK: clazz init [[C:.*]]
    try! await printTaskLocal(\.clazz) // CHECK: ClazzKey: Optional(main.ClassTaskLocal) {{.*}}
  }
  // CHECK: clazz deinit [[C]]
  try! await printTaskLocal(\.clazz) // CHECK: ClazzKey: nil {{.*}}
}

func nested() async {
  try! await printTaskLocal(\.string) // CHECK: StringKey: <undefined> {{.*}}
  await Task.withLocal(\.string, boundTo: "hello") {
    try! await printTaskLocal(\.number) // CHECK-NEXT: NumberKey: 0 {{.*}}
    try! await printTaskLocal(\.string)// CHECK-NEXT: StringKey: hello {{.*}}
    await Task.withLocal(\.number, boundTo: 2) {
      try! await printTaskLocal(\.number) // CHECK-NEXT: NumberKey: 2 {{.*}}
      try! await printTaskLocal(\.string, "hello") // CHECK: StringKey: hello {{.*}}
    }
    try! await printTaskLocal(\.number) // CHECK-NEXT: NumberKey: 0 {{.*}}
    try! await printTaskLocal(\.string) // CHECK-NEXT: StringKey: hello {{.*}}
  }
  try! await printTaskLocal(\.number) // CHECK-NEXT: NumberKey: 0 {{.*}}
  try! await printTaskLocal(\.string) // CHECK-NEXT: StringKey: <undefined> {{.*}}
}

func nested_allContribute() async {
  try! await printTaskLocal(\.string) // CHECK: StringKey: <undefined> {{.*}}
  await Task.withLocal(\.string, boundTo: "one") {
    try! await printTaskLocal(\.string, "one")// CHECK-NEXT: StringKey: one {{.*}}
    await Task.withLocal(\.string, boundTo: "two") {
      try! await printTaskLocal(\.string, "two") // CHECK-NEXT: StringKey: two {{.*}}
      await Task.withLocal(\.string, boundTo: "three") {
        try! await printTaskLocal(\.string, "three") // CHECK-NEXT: StringKey: three {{.*}}
      }
      try! await printTaskLocal(\.string, "two") // CHECK-NEXT: StringKey: two {{.*}}
    }
    try! await printTaskLocal(\.string, "one")// CHECK-NEXT: StringKey: one {{.*}}
  }
  try! await printTaskLocal(\.string) // CHECK-NEXT: StringKey: <undefined> {{.*}}
}

func nested_3_onlyTopContributes() async {
  try! await printTaskLocal(\.string) // CHECK: StringKey: <undefined> {{.*}}
  await Task.withLocal(\.string, boundTo: "one") {
    try! await printTaskLocal(\.string)// CHECK-NEXT: StringKey: one {{.*}}
    await Task.withLocal(\.number, boundTo: 2) {
      try! await printTaskLocal(\.string) // CHECK-NEXT: StringKey: one {{.*}}
      await Task.withLocal(\.number, boundTo: 3) {
        try! await printTaskLocal(\.string) // CHECK-NEXT: StringKey: one {{.*}}
      }
      try! await printTaskLocal(\.string) // CHECK-NEXT: StringKey: one {{.*}}
    }
    try! await printTaskLocal(\.string)// CHECK-NEXT: StringKey: one {{.*}}
  }
  try! await printTaskLocal(\.string) // CHECK-NEXT: StringKey: <undefined> {{.*}}
}

@main struct Main {
  static func main() async {
    await simple()
    await simple_deinit()
    await nested()
    await nested_allContribute()
    await nested_3_onlyTopContributes()
  }
}
