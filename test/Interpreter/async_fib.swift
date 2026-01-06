// RUN: %empty-directory(%t)
// RUN: %target-build-swift  -target %target-swift-5.1-abi-triple %s -parse-as-library -module-name main -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: OS=windows-msvc

var gg = 0

@inline(never)
public func identity<T>(_ x: T) -> T {
  gg += 1
  return x
}

actor Actor {
  var x: Int = 0
  init(x: Int) { self.x = x }

  @inline(never)
  func get(_ i: Int ) async -> Int {
    return i + x
  }
}

// Used to crash with an stack overflow with m >= 18
let m = 22

@inline(never)
func asyncFib(_ n: Int, _ a1: Actor, _ a2: Actor) async -> Int {
  if n == 0 {
    return await a1.get(n)
  }
  if n == 1 {
    return await a2.get(n)
  }

  let first = await asyncFib(n-2, a1, a2)
  let second = await asyncFib(n-1, a1, a2)

  let result = first + second

  return result
}

@main struct Main {
  static func main() async {
    let a1 = Actor(x: 0)
    let a2 = Actor(x: 0)
    _ = await asyncFib(identity(m), a1, a2)

    // CHECK: result: 0
    await print("result: \(a1.x)");
    await print(a2.x)
  }
}
