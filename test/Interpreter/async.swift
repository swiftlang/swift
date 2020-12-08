// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -enable-experimental-concurrency %s -module-name main -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: CPU=arm64e


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

runAsyncAndBlock {
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
