// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency -parse-as-library %import-libdispatch) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

final class StringLike: Sendable, CustomStringConvertible {
  let value: String
  init(_ value: String) {
    self.value = value
  }

  var description: String { value }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
enum TL {

  @TaskLocal(default: "<undefined>")
  static var string

  @TaskLocal(default: 0)
  static var number

  @TaskLocal(default: StringLike("<never>"))
  static var never

  @TaskLocal()
  static var clazz: TaskLocal<ClassTaskLocal?>.Access
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
final class ClassTaskLocal: Sendable {
  init() {
    print("clazz init \(ObjectIdentifier(self))")
  }

  deinit {
    print("clazz deinit \(ObjectIdentifier(self))")
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@discardableResult
func printTaskLocal<V>(
    _ key: TaskLocal<V>.Access,
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
func simple() async {
  printTaskLocal(TL.number) // CHECK: TaskLocal<Int>.Access (0)
  await TL.number.withValue(1) {
    printTaskLocal(TL.number) // CHECK-NEXT: TaskLocal<Int>.Access (1)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func simple_deinit() async {
  await TL.clazz.withValue(ClassTaskLocal()) {
    // CHECK: clazz init [[C:.*]]
    printTaskLocal(TL.clazz) // CHECK: TaskLocal<Optional<ClassTaskLocal>>.Access (Optional(main.ClassTaskLocal))
  }
  // CHECK: clazz deinit [[C]]
  printTaskLocal(TL.clazz) // CHECK: TaskLocal<Optional<ClassTaskLocal>>.Access (nil)
}

struct Boom: Error {
  let value: String
}
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func simple_throw() async {
  do {
    try await TL.clazz.withValue(ClassTaskLocal()) {
      throw Boom(value: "oh no!")
    }
  } catch {
    //CHECK: error: Boom(value: "oh no!")
    print("error: \(error)")
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func nested() async {
  printTaskLocal(TL.string) // CHECK: TaskLocal<String>.Access (<undefined>)
  await TL.string.withValue("hello") {
    printTaskLocal(TL.number) // CHECK-NEXT: TaskLocal<Int>.Access (0)
    printTaskLocal(TL.string)// CHECK-NEXT: TaskLocal<String>.Access (hello)
    await TL.number.withValue(2) {
      printTaskLocal(TL.number) // CHECK-NEXT: TaskLocal<Int>.Access (2)
      printTaskLocal(TL.string, "hello") // CHECK: TaskLocal<String>.Access (hello)
    }
    printTaskLocal(TL.number) // CHECK-NEXT: TaskLocal<Int>.Access (0)
    printTaskLocal(TL.string) // CHECK-NEXT: TaskLocal<String>.Access (hello)
  }
  printTaskLocal(TL.number) // CHECK-NEXT: TaskLocal<Int>.Access (0)
  printTaskLocal(TL.string) // CHECK-NEXT: TaskLocal<String>.Access (<undefined>)
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func nested_allContribute() async {
  printTaskLocal(TL.string) // CHECK: TaskLocal<String>.Access (<undefined>)
  await TL.string.withValue("one") {
    printTaskLocal(TL.string, "one")// CHECK-NEXT: TaskLocal<String>.Access (one)
    await TL.string.withValue("two") {
      printTaskLocal(TL.string, "two") // CHECK-NEXT: TaskLocal<String>.Access (two)
      await TL.string.withValue("three") {
        printTaskLocal(TL.string, "three") // CHECK-NEXT: TaskLocal<String>.Access (three)
      }
      printTaskLocal(TL.string, "two") // CHECK-NEXT: TaskLocal<String>.Access (two)
    }
    printTaskLocal(TL.string, "one")// CHECK-NEXT: TaskLocal<String>.Access (one)
  }
  printTaskLocal(TL.string) // CHECK-NEXT: TaskLocal<String>.Access (<undefined>)
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func nested_3_onlyTopContributes() async {
  printTaskLocal(TL.string) // CHECK: TaskLocal<String>.Access (<undefined>)
  await TL.string.withValue("one") {
    printTaskLocal(TL.string)// CHECK-NEXT: TaskLocal<String>.Access (one)
    await TL.number.withValue(2) {
      printTaskLocal(TL.string) // CHECK-NEXT: TaskLocal<String>.Access (one)
      await TL.number.withValue(3) {
        printTaskLocal(TL.string) // CHECK-NEXT: TaskLocal<String>.Access (one)
      }
      printTaskLocal(TL.string) // CHECK-NEXT: TaskLocal<String>.Access (one)
    }
    printTaskLocal(TL.string)// CHECK-NEXT: TaskLocal<String>.Access (one)
  }
  printTaskLocal(TL.string) // CHECK-NEXT: TaskLocal<String>.Access (<undefined>)
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func withLocal_body_mustNotEscape() async {
  var something = "Nice"
  await TL.string.withValue("xxx") {
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
