// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

enum TL {

  @TaskLocal
  static var string: String = "<undefined>"

  @TaskLocal
  static var number: Int = 0

  @TaskLocal
  static var clazz: ClassTaskLocal?
}

final class ClassTaskLocal: Sendable {
  init() {
    print("clazz init \(ObjectIdentifier(self))")
  }

  deinit {
    print("clazz deinit \(ObjectIdentifier(self))")
  }
}

@discardableResult
func printTaskLocalAsync<V>(
    _ key: TaskLocal<V>,
    _ expected: V? = nil,
    file: String = #file, line: UInt = #line
) async -> V? {
  printTaskLocal(key, expected, file: file, line: line)
}

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

func simple() async {
  printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (0)
  TL.$number.unsafePushValue(1)
  defer { printTaskLocal(TL.$number) }
  defer { TL.$number.unsafePopValue() }
  printTaskLocal(TL.$number) // CHECK-NEXT: TaskLocal<Int>(defaultValue: 0) (1)
  // defer: pop
  // defer: print // CHECK-NEXT: TaskLocal<Int>(defaultValue: 0) (0)
}

func simple_deinit() async {
  TL.$clazz.unsafePushValue(ClassTaskLocal())
  // CHECK: clazz init [[C:.*]]
  printTaskLocal(TL.$clazz) // CHECK: TaskLocal<Optional<ClassTaskLocal>>(defaultValue: nil) (Optional(main.ClassTaskLocal))
  TL.$clazz.unsafePopValue() // release SPECIFICALLY here

  // CHECK: clazz deinit [[C]]
  printTaskLocal(TL.$clazz) // CHECK: TaskLocal<Optional<ClassTaskLocal>>(defaultValue: nil) (nil)
}

func nested() async {
  printTaskLocal(TL.$string) // CHECK: TaskLocal<String>(defaultValue: <undefined>) (<undefined>)
  TL.$string.unsafePushValue("hello")
  do {
    printTaskLocal(TL.$number) // CHECK-NEXT: TaskLocal<Int>(defaultValue: 0) (0)
    printTaskLocal(TL.$string)// CHECK-NEXT: TaskLocal<String>(defaultValue: <undefined>) (hello)
    TL.$number.unsafePushValue(2)
    do {
      printTaskLocal(TL.$number) // CHECK-NEXT: TaskLocal<Int>(defaultValue: 0) (2)
      printTaskLocal(TL.$string, "hello") // CHECK: TaskLocal<String>(defaultValue: <undefined>) (hello)
    }
    TL.$number.unsafePopValue() // pop "2"
    printTaskLocal(TL.$number) // CHECK-NEXT: TaskLocal<Int>(defaultValue: 0) (0)
    printTaskLocal(TL.$string) // CHECK-NEXT: TaskLocal<String>(defaultValue: <undefined>) (hello)
  }
  TL.$string.unsafePopValue() // pop "hello"
  printTaskLocal(TL.$number) // CHECK-NEXT: TaskLocal<Int>(defaultValue: 0) (0)
  printTaskLocal(TL.$string) // CHECK-NEXT: TaskLocal<String>(defaultValue: <undefined>) (<undefined>)
}


@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async {
    await simple()
    await simple_deinit()
    await nested()
  }
}
