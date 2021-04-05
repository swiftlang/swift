// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency %import-libdispatch -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency

// rdar://76038845
// UNSUPPORTED: use_os_stdlib

// UNSUPPORTED: OS=windows-msvc

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func simple() async {
  print("\(#function) -----------------------")
  let one = Task.current!
  let two = Task.current!
  print("same equal: \(one == two)") // CHECK: same equal: true
  print("hashes equal: \(one.hashValue == two.hashValue)") // CHECK: hashes equal: true

  async let x = Task.current
  let three = await x

  print("parent/child equal: \(three == two)") // CHECK: parent/child equal: false
  print("parent/child hashes equal: \(three.hashValue == two.hashValue)") // CHECK: parent/child hashes equal: false
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func unsafe() async {
  print("\(#function) -----------------------")
  let one = withUnsafeCurrentTask { $0! }
  let two = withUnsafeCurrentTask { $0! }
  print("unsafe same equal: \(one == two)") // CHECK: same equal: true
  print("unsafe hashes equal: \(one.hashValue == two.hashValue)") // CHECK: hashes equal: true

  async let x = withUnsafeCurrentTask { $0! }
  let three = await x

  print("unsafe parent/child equal: \(three == two)") // CHECK: parent/child equal: false
  print("unsafe parent/child hashes equal: \(three.hashValue == two.hashValue)") // CHECK: parent/child hashes equal: false

  print("unsafe.task parent/child equal: \(three.task == two.task)") // CHECK: parent/child equal: false
  print("unsafe.task parent/child hashes equal: \(three.task.hashValue == two.task.hashValue)") // CHECK: parent/child hashes equal: false
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func unsafeSync() {
  print("\(#function) -----------------------")
  let one = withUnsafeCurrentTask { $0! }
  let two = withUnsafeCurrentTask { $0! }
  print("unsafe same equal: \(one == two)") // CHECK: same equal: true
  print("unsafe hashes equal: \(one.hashValue == two.hashValue)") // CHECK: hashes equal: true
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@main struct Main {
  static func main() async {
    await simple()
    await unsafe()
    unsafeSync()
  }
}
