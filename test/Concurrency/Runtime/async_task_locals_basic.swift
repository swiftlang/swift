// RUN: %target-run-simple-swift( -plugin-path %swift-plugin-dir -parse-as-library %import-libdispatch) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

final class StringLike: Sendable, ExpressibleByStringLiteral, CustomStringConvertible {
  let value: String
  init(_ value: String) {
    self.value = value
  }
  init(stringLiteral value: StringLiteralType) {
    self.value = value
  }

  var description: String { value }
}

@available(SwiftStdlib 5.1, *)
enum TL {

  @TaskLocal
  static var string: String = "<undefined>"

  @TaskLocal
  static var number: Int = 0

  @TaskLocal
  static var never: StringLike = StringLike("<never>")

  @TaskLocal
  static var clazz: ClassTaskLocal?

  @TaskLocal
  static var force: ForceUnwrapMe!
}

@TaskLocal
@available(SwiftStdlib 5.1, *)
var globalTaskLocal: StringLike = StringLike("<not-set>")

@available(SwiftStdlib 5.10, *)
struct LessAvailable {}

struct ForceUnwrapMe {}

@TaskLocal
@available(SwiftStdlib 5.10, *)
var globalLessAvailable: LessAvailable?

@available(SwiftStdlib 5.1, *)
final class ClassTaskLocal: Sendable {
  init() {
    print("clazz init \(ObjectIdentifier(self))")
  }

  deinit {
    print("clazz deinit \(ObjectIdentifier(self))")
  }
}

@available(SwiftStdlib 5.1, *)
@discardableResult
func printTaskLocalAsync<V>(
    _ key: TaskLocal<V>,
    _ expected: V? = nil,
    file: String = #file, line: UInt = #line
) async -> V? {
  printTaskLocal(key, expected, file: file, line: line)
}

@available(SwiftStdlib 5.1, *)
@discardableResult
func printTaskLocal<V>(
    _ key: TaskLocal<V>,
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

@available(SwiftStdlib 5.1, *)
func simple() async {
  printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (0)
  TL.$number.withValue(1) {
    printTaskLocal(TL.$number) // CHECK-NEXT: TaskLocal<Int>(defaultValue: 0) (1)
  }

  // async body
  await TL.$number.withValue(1) {
    await Task.yield()
    print("OK: \(TL.number)")
  }

  // sync body
  TL.$number.withValue(1) {
    print("OK: \(TL.number)")
  }
}

@available(SwiftStdlib 5.1, *)
func simple_deinit() async {
  TL.$clazz.withValue(ClassTaskLocal()) {
    // CHECK: clazz init [[C:.*]]
    printTaskLocal(TL.$clazz) // CHECK: TaskLocal<Optional<ClassTaskLocal>>(defaultValue: nil) (Optional(main.ClassTaskLocal))
  }
  // CHECK: clazz deinit [[C]]
  printTaskLocal(TL.$clazz) // CHECK: TaskLocal<Optional<ClassTaskLocal>>(defaultValue: nil) (nil)
}

struct Boom: Error {
  let value: String
}
@available(SwiftStdlib 5.1, *)
func simple_throw() async {
  do {
    try TL.$clazz.withValue(ClassTaskLocal()) {
      throw Boom(value: "oh no!")
    }
  } catch {
    //CHECK: error: Boom(value: "oh no!")
    print("error: \(error)")
  }
}

@available(SwiftStdlib 5.1, *)
func nested() async {
  printTaskLocal(TL.$string) // CHECK: TaskLocal<String>(defaultValue: <undefined>) (<undefined>)
  TL.$string.withValue("hello") {
    printTaskLocal(TL.$number) // CHECK-NEXT: TaskLocal<Int>(defaultValue: 0) (0)
    printTaskLocal(TL.$string)// CHECK-NEXT: TaskLocal<String>(defaultValue: <undefined>) (hello)
    TL.$number.withValue(2) {
      printTaskLocal(TL.$number) // CHECK-NEXT: TaskLocal<Int>(defaultValue: 0) (2)
      printTaskLocal(TL.$string, "hello") // CHECK: TaskLocal<String>(defaultValue: <undefined>) (hello)
    }
    printTaskLocal(TL.$number) // CHECK-NEXT: TaskLocal<Int>(defaultValue: 0) (0)
    printTaskLocal(TL.$string) // CHECK-NEXT: TaskLocal<String>(defaultValue: <undefined>) (hello)
  }
  printTaskLocal(TL.$number) // CHECK-NEXT: TaskLocal<Int>(defaultValue: 0) (0)
  printTaskLocal(TL.$string) // CHECK-NEXT: TaskLocal<String>(defaultValue: <undefined>) (<undefined>)
}

@available(SwiftStdlib 5.1, *)
func nested_allContribute() async {
  printTaskLocal(TL.$string) // CHECK: TaskLocal<String>(defaultValue: <undefined>) (<undefined>)
  TL.$string.withValue("one") {
    printTaskLocal(TL.$string, "one")// CHECK-NEXT: TaskLocal<String>(defaultValue: <undefined>) (one)
    TL.$string.withValue("two") {
      printTaskLocal(TL.$string, "two") // CHECK-NEXT: TaskLocal<String>(defaultValue: <undefined>) (two)
      TL.$string.withValue("three") {
        printTaskLocal(TL.$string, "three") // CHECK-NEXT: TaskLocal<String>(defaultValue: <undefined>) (three)
      }
      printTaskLocal(TL.$string, "two") // CHECK-NEXT: TaskLocal<String>(defaultValue: <undefined>) (two)
    }
    printTaskLocal(TL.$string, "one")// CHECK-NEXT: TaskLocal<String>(defaultValue: <undefined>) (one)
  }
  printTaskLocal(TL.$string) // CHECK-NEXT: TaskLocal<String>(defaultValue: <undefined>) (<undefined>)
}

@available(SwiftStdlib 5.1, *)
func nested_3_onlyTopContributes() async {
  printTaskLocal(TL.$string) // CHECK: TaskLocal<String>(defaultValue: <undefined>) (<undefined>)
  TL.$string.withValue("one") {
    printTaskLocal(TL.$string)// CHECK-NEXT: TaskLocal<String>(defaultValue: <undefined>) (one)
    TL.$number.withValue(2) {
      printTaskLocal(TL.$string) // CHECK-NEXT: TaskLocal<String>(defaultValue: <undefined>) (one)
      TL.$number.withValue(3) {
        printTaskLocal(TL.$string) // CHECK-NEXT: TaskLocal<String>(defaultValue: <undefined>) (one)
      }
      printTaskLocal(TL.$string) // CHECK-NEXT: TaskLocal<String>(defaultValue: <undefined>) (one)
    }
    printTaskLocal(TL.$string)// CHECK-NEXT: TaskLocal<String>(defaultValue: <undefined>) (one)
  }
  printTaskLocal(TL.$string) // CHECK-NEXT: TaskLocal<String>(defaultValue: <undefined>) (<undefined>)
}

@available(SwiftStdlib 5.1, *)
func nested_3_onlyTopContributesAsync() async {
  await printTaskLocalAsync(TL.$string) // CHECK: TaskLocal<String>(defaultValue: <undefined>) (<undefined>)
  await TL.$string.withValue("one") {
    await printTaskLocalAsync(TL.$string)// CHECK-NEXT: TaskLocal<String>(defaultValue: <undefined>) (one)
    await TL.$number.withValue(2) {
      await printTaskLocalAsync(TL.$string) // CHECK-NEXT: TaskLocal<String>(defaultValue: <undefined>) (one)
      await TL.$number.withValue(3) {
        await printTaskLocalAsync(TL.$string) // CHECK-NEXT: TaskLocal<String>(defaultValue: <undefined>) (one)
      }
      await printTaskLocalAsync(TL.$string) // CHECK-NEXT: TaskLocal<String>(defaultValue: <undefined>) (one)
    }
    await printTaskLocalAsync(TL.$string)// CHECK-NEXT: TaskLocal<String>(defaultValue: <undefined>) (one)
  }
  await printTaskLocalAsync(TL.$string) // CHECK-NEXT: TaskLocal<String>(defaultValue: <undefined>) (<undefined>)
}

@available(SwiftStdlib 5.1, *)
func nested_3_onlyTopContributesMixed() async {
  await printTaskLocalAsync(TL.$string) // CHECK: TaskLocal<String>(defaultValue: <undefined>) (<undefined>)
  await TL.$string.withValue("one") {
    await printTaskLocalAsync(TL.$string)// CHECK-NEXT: TaskLocal<String>(defaultValue: <undefined>) (one)
    await TL.$number.withValue(2) {
      printTaskLocal(TL.$string) // CHECK-NEXT: TaskLocal<String>(defaultValue: <undefined>) (one)
      TL.$number.withValue(3) {
        printTaskLocal(TL.$string) // CHECK-NEXT: TaskLocal<String>(defaultValue: <undefined>) (one)
      }
      await printTaskLocalAsync(TL.$string) // CHECK-NEXT: TaskLocal<String>(defaultValue: <undefined>) (one)
    }
    await printTaskLocalAsync(TL.$string)// CHECK-NEXT: TaskLocal<String>(defaultValue: <undefined>) (one)
  }
  await printTaskLocalAsync(TL.$string) // CHECK-NEXT: TaskLocal<String>(defaultValue: <undefined>) (<undefined>)
}

@available(SwiftStdlib 5.1, *)
func withLocal_body_mustNotEscape() async {
  var something = "Nice"
  TL.$string.withValue("xxx") {
    something = "very nice"
  }
  _ = something // silence not used warning
}

@available(SwiftStdlib 5.1, *)
actor Worker {
  @TaskLocal
  static var declaredInActor: String = "<default-value>"

  func setAndRead() async {
    print("setAndRead") // CHECK: setAndRead
    await Worker.$declaredInActor.withValue("value-1") {
      await printTaskLocalAsync(Worker.$declaredInActor) // CHECK-NEXT: TaskLocal<String>(defaultValue: <default-value>) (value-1)
    }
  }
}

@available(SwiftStdlib 5.1, *)
func inside_actor() async {
  await Worker().setAndRead()
}

@available(SwiftStdlib 5.1, *)
func global_task_local() async {
  await $globalTaskLocal.withValue("value-1") {
    await printTaskLocalAsync($globalTaskLocal) // CHECK-NEXT: TaskLocal<StringLike>(defaultValue: <not-set>) (value-1)
  }
}

@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async {
    await simple()
    await simple_deinit()
    await simple_throw()
    await nested()
    await nested_allContribute()
    await nested_3_onlyTopContributes()
    await nested_3_onlyTopContributesAsync()
    await nested_3_onlyTopContributesMixed()
    await inside_actor()
    await global_task_local()
  }
}
