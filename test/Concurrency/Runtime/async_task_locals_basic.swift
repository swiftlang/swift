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

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
final class ClassTaskLocal {
  init() {
    print("clazz init \(ObjectIdentifier(self))")
  }

  deinit {
    print("clazz deinit \(ObjectIdentifier(self))")
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func printTaskLocal<Key>(
  _ key: KeyPath<TaskLocalValues, Key>,
  _ expected: Key.Value? = nil,
  file: String = #file, line: UInt = #line, function: String = #function
) where Key: TaskLocalKey {
  let value = Task.local(key)
  print("\(Key.self): \(value) at \(file):\(line)")
  if let expected = expected {
    assert("\(expected)" == "\(value)",
      "Expected [\(expected)] but found: \(value), at \(file):\(line)")
  }
}

// ==== ------------------------------------------------------------------------

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func simple() async {
  printTaskLocal(\.number) // CHECK: NumberKey: 0 {{.*}}
  await Task.withLocal(\.number, boundTo: 1) {
    printTaskLocal(\.number) // CHECK-NEXT: NumberKey: 1 {{.*}}
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func simple_deinit() async {
  await Task.withLocal(\.clazz, boundTo: ClassTaskLocal()) {
    // CHECK: clazz init [[C:.*]]
    printTaskLocal(\.clazz) // CHECK: ClazzKey: Optional(main.ClassTaskLocal) {{.*}}
  }
  // CHECK: clazz deinit [[C]]
  printTaskLocal(\.clazz) // CHECK: ClazzKey: nil {{.*}}
}

struct Boom: Error {
  let value: String
}
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func simple_throw() async {
  do {
    try await Task.withLocal(\.clazz, boundTo: ClassTaskLocal()) {
      throw Boom(value: "oh no!")
    }
  } catch {
    //CHECK: error: Boom(value: "oh no!")
    print("error: \(error)")
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func nested() async {
  printTaskLocal(\.string) // CHECK: StringKey: <undefined> {{.*}}
  await Task.withLocal(\.string, boundTo: "hello") {
    printTaskLocal(\.number) // CHECK-NEXT: NumberKey: 0 {{.*}}
    printTaskLocal(\.string)// CHECK-NEXT: StringKey: hello {{.*}}
    await Task.withLocal(\.number, boundTo: 2) {
      printTaskLocal(\.number) // CHECK-NEXT: NumberKey: 2 {{.*}}
      printTaskLocal(\.string, "hello") // CHECK: StringKey: hello {{.*}}
    }
    printTaskLocal(\.number) // CHECK-NEXT: NumberKey: 0 {{.*}}
    printTaskLocal(\.string) // CHECK-NEXT: StringKey: hello {{.*}}
  }
  printTaskLocal(\.number) // CHECK-NEXT: NumberKey: 0 {{.*}}
  printTaskLocal(\.string) // CHECK-NEXT: StringKey: <undefined> {{.*}}
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func nested_allContribute() async {
  printTaskLocal(\.string) // CHECK: StringKey: <undefined> {{.*}}
  await Task.withLocal(\.string, boundTo: "one") {
    printTaskLocal(\.string, "one")// CHECK-NEXT: StringKey: one {{.*}}
    await Task.withLocal(\.string, boundTo: "two") {
      printTaskLocal(\.string, "two") // CHECK-NEXT: StringKey: two {{.*}}
      await Task.withLocal(\.string, boundTo: "three") {
        printTaskLocal(\.string, "three") // CHECK-NEXT: StringKey: three {{.*}}
      }
      printTaskLocal(\.string, "two") // CHECK-NEXT: StringKey: two {{.*}}
    }
    printTaskLocal(\.string, "one")// CHECK-NEXT: StringKey: one {{.*}}
  }
  printTaskLocal(\.string) // CHECK-NEXT: StringKey: <undefined> {{.*}}
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func nested_3_onlyTopContributes() async {
  printTaskLocal(\.string) // CHECK: StringKey: <undefined> {{.*}}
  await Task.withLocal(\.string, boundTo: "one") {
    printTaskLocal(\.string)// CHECK-NEXT: StringKey: one {{.*}}
    await Task.withLocal(\.number, boundTo: 2) {
      printTaskLocal(\.string) // CHECK-NEXT: StringKey: one {{.*}}
      await Task.withLocal(\.number, boundTo: 3) {
        printTaskLocal(\.string) // CHECK-NEXT: StringKey: one {{.*}}
      }
      printTaskLocal(\.string) // CHECK-NEXT: StringKey: one {{.*}}
    }
    printTaskLocal(\.string)// CHECK-NEXT: StringKey: one {{.*}}
  }
  printTaskLocal(\.string) // CHECK-NEXT: StringKey: <undefined> {{.*}}
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func withLocal_body_mustNotEscape() async {
  var something = "Nice"
  await Task.withLocal(\.string, boundTo: "xxx") {
    something = "very nice"
  }
  _ = something // silence not used warning
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@main struct Main {
  static func main() async {
    await simple()
    await simple_deinit()
    await simple_throw()
    await nested()
    await nested_allContribute()
    await nested_3_onlyTopContributes()
  }
}
