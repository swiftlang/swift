// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o -plugin-path %swift-plugin-dir
// RUN: %target-clang %t/a.o %target-embedded-posix-shim -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-clang-resource-dir-opt -lc++abi -lswift_Concurrency %target-swift-default-executor-opt -dead_strip -Xlinker %swift_obj_root/lib/swift/embedded/%module-target-triple/libswiftUnicodeDataTables.a
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: concurrency
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded
// REQUIRES: concurrency_runtime

import _Concurrency

final class StringLike: Sendable, ExpressibleByStringLiteral, CustomStringConvertible, Equatable {
  let value: String
  init(_ value: String) {
    self.value = value
  }
  init(stringLiteral value: String) {
    self.value = value
  }

  var description: String { value }

  static func ==(lhs: StringLike, rhs: StringLike) -> Bool {
    return lhs.value == rhs.value
  }
}

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
var globalTaskLocal: StringLike = StringLike("<not-set>")

struct LessAvailable {}

struct ForceUnwrapMe {}

@TaskLocal
var globalLessAvailable: LessAvailable?

final class ClassTaskLocal: Sendable, Equatable {
  init() {
    print("clazz init \(ObjectIdentifier(self))")
  }

  deinit {
    print("clazz deinit \(ObjectIdentifier(self))")
  }

  static func ==(lhs: ClassTaskLocal, rhs: ClassTaskLocal) -> Bool {
    true
  }
}

@discardableResult
func checkTaskLocalAsync<V: Equatable>(
    _ key: TaskLocal<V>,
    _ expected: V,
    file: String = #file, line: UInt = #line
) async -> V {
  checkTaskLocal(key, expected, file: file, line: line)
}

@discardableResult
func checkTaskLocal<V: Equatable>(
    _ key: TaskLocal<V>,
    _ expected: V,
    file: String = #file, line: UInt = #line
) -> V {
  let value = key.get()
  precondition(value == expected)
  return expected
}

// ==== ------------------------------------------------------------------------

func simple() async {
  checkTaskLocal(TL.$number, 0)
  TL.$number.withValue(1) {
    checkTaskLocal(TL.$number, 1)
  }

  // async body
  await TL.$number.withValue(1) {
    await Task.yield()
    print("OK: \(TL.number)") // CHECK: OK: 1
  }

  // sync body
  TL.$number.withValue(1) {
    print("OK: \(TL.number)") // CHECK-NEXT: OK: 1
  }
}

func simple_deinit() async {
  TL.$clazz.withValue(ClassTaskLocal()) {
    // CHECK: clazz init [[C:.*]]
    checkTaskLocal(TL.$clazz, ClassTaskLocal())
  }
  // CHECK: clazz deinit [[C]]
  checkTaskLocal(TL.$clazz, nil)
}

struct Boom: Error {
  let value: String
}

func simple_throw() async {
  do {
    try TL.$clazz.withValue(ClassTaskLocal()) {
      throw Boom(value: "oh no!")
    }
  } catch {
    //CHECK: error: (cannot print value
    print("error: \(error)")
  }
}

func nested() async {
  checkTaskLocal(TL.$string, "<undefined>")
  TL.$string.withValue("hello") {
    checkTaskLocal(TL.$number, 0)
    checkTaskLocal(TL.$string, "hello")
    TL.$number.withValue(2) {
      checkTaskLocal(TL.$number, 2)
      checkTaskLocal(TL.$string, "hello")
    }
    checkTaskLocal(TL.$number, 0)
    checkTaskLocal(TL.$string, "hello")
  }
  checkTaskLocal(TL.$number, 0)
  checkTaskLocal(TL.$string, "<undefined>")
}

func nested_allContribute() async {
  checkTaskLocal(TL.$string, "<undefined>")
  TL.$string.withValue("one") {
    checkTaskLocal(TL.$string, "one")
    TL.$string.withValue("two") {
      checkTaskLocal(TL.$string, "two")
      TL.$string.withValue("three") {
        checkTaskLocal(TL.$string, "three")
      }
      checkTaskLocal(TL.$string, "two")
    }
    checkTaskLocal(TL.$string, "one")
  }
  checkTaskLocal(TL.$string, "<undefined>")
}

func nested_3_onlyTopContributes() async {
  checkTaskLocal(TL.$string, "<undefined>")
  TL.$string.withValue("one") {
    checkTaskLocal(TL.$string, "one")
    TL.$number.withValue(2) {
      checkTaskLocal(TL.$string, "one")
      TL.$number.withValue(3) {
        checkTaskLocal(TL.$string, "one")
      }
      checkTaskLocal(TL.$string, "one")
    }
    checkTaskLocal(TL.$string, "one")
  }
  checkTaskLocal(TL.$string, "<undefined>")
}

func nested_3_onlyTopContributesAsync() async {
  await checkTaskLocalAsync(TL.$string, "<undefined>")
  await TL.$string.withValue("one") {
    await checkTaskLocalAsync(TL.$string, "one")
    await TL.$number.withValue(2) {
      await checkTaskLocalAsync(TL.$string, "one")
      await TL.$number.withValue(3) {
        await checkTaskLocalAsync(TL.$string, "one")
      }
      await checkTaskLocalAsync(TL.$string, "one")
    }
    await checkTaskLocalAsync(TL.$string, "one")
  }
  await checkTaskLocalAsync(TL.$string, "<undefined>")
}

func nested_3_onlyTopContributesMixed() async {
  await checkTaskLocalAsync(TL.$string, "<undefined>")
  await TL.$string.withValue("one") {
    await checkTaskLocalAsync(TL.$string, "one")
    await TL.$number.withValue(2) {
      checkTaskLocal(TL.$string, "one")
      TL.$number.withValue(3) {
        checkTaskLocal(TL.$string, "one")
      }
      await checkTaskLocalAsync(TL.$string, "one")
    }
    await checkTaskLocalAsync(TL.$string, "one")
  }
  await checkTaskLocalAsync(TL.$string, "<undefined>")
}

func withLocal_body_mustNotEscape() async {
  var something = "Nice"
  TL.$string.withValue("xxx") {
    something = "very nice"
  }
  _ = something // silence not used warning
}

actor Worker {
  @TaskLocal
  static var declaredInActor: String = "<default-value>"

  func setAndRead() async {
    print("setAndRead") // CHECK: setAndRead
    await Worker.$declaredInActor.withValue("value-1") {
      await checkTaskLocalAsync(Worker.$declaredInActor, "value-1")
    }
  }
}

func inside_actor() async {
  await Worker().setAndRead()
}

func global_task_local() async {
  await $globalTaskLocal.withValue("value-1") {
    await checkTaskLocalAsync($globalTaskLocal, "value-1")
  }
}

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
