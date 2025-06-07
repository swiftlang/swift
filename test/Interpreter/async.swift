// RUN: %empty-directory(%t)
// RUN: %target-build-swift  -target %target-swift-5.1-abi-triple %s -parse-as-library -module-name main -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime


func sayHello() async {
  print("hello")
}

func sayGeneric<T>(_ msg: T) async {
  await sayHello()
  print(msg)
}

func sayWithClosure(_ action: () async -> ()) async {
  await action()
  print("hallo welt")
}

@main struct Main {
  static func main() async {
    // CHECK: hello
    await sayHello()

    // CHECK: hello
    // CHECK: world
    await sayGeneric("world")


    // CHECK: hello
    // CHECK: and now in german
    // CHECK: hallo welt
    await sayWithClosure {
      await sayHello()
      print("and now in german")
    }
  }
}
