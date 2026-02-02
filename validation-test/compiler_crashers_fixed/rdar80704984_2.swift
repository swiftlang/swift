// RUN: %empty-directory(%t)
// RUN: %target-clang %S/Inputs/rdar80704984_2.m -I %S/Inputs -c -o %t/rdar80704984_2.o
// RUN: %target-build-swift -Xfrontend -disable-availability-checking -import-objc-header %S/Inputs/rdar80704984_2.h -Xlinker %t/rdar80704984_2.o -parse-as-library %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

// rdar://82123254
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

func run1(on object: PFXObject) async throws {
  do {
    try await object.failReturn()
    fatalError()
  }
  catch let error {
    // CHECK: Error Domain=failReturn Code=1 "(null)"
    print(error)
  }
}

func run2(on object: PFXObject) async throws {
  do {
    try await object.failInvokeSync()
    fatalError()
  }
  catch let error {
    // CHECK: Error Domain=failInvokeSync Code=2 "(null)"
    print(error)
  }
}

func run3(on object: PFXObject) async throws {
  do {
    try await object.failInvokeAsync()
    fatalError()
  }
  catch let error {
    // CHECK: Error Domain=failInvokeAsync Code=2 "(null)"
    print(error)
  }
}

func run4(on object: PFXObject) async throws {
  // CHECK: ()
  print(try await object.succeedSync())
}

func run5(on object: PFXObject) async throws {
  // CHECK: ()
  print(try await object.succeedAsync())
}

func runAll(on object: PFXObject) async throws {
  do {
    try await object.failReturn()
    fatalError()
  }
  catch let error {
    // CHECK: Error Domain=failReturn Code=1 "(null)"
    print(error)
  }
  do {
    try await object.failInvokeSync()
    fatalError()
  }
  catch let error {
    // CHECK: Error Domain=failInvokeSync Code=2 "(null)"
    print(error)
  }
  do {
    try await object.failInvokeAsync()
    fatalError()
  }
  catch let error {
    // CHECK: Error Domain=failInvokeAsync Code=2 "(null)"
    print(error)
  }
  // CHECK: ()
  print(try await object.succeedSync())
  // CHECK: ()
  print(try await object.succeedAsync())
}

@main struct Main {
  static func main() async throws {
    let object = PFXObject()
    try await run1(on: object)
    try await run2(on: object)
    try await run3(on: object)
    try await run4(on: object)
    try await run5(on: object)
    try await runAll(on: object)
  }
}
