// RUN: %empty-directory(%t)
// RUN: %target-clang %S/Inputs/rdar80704984.m -I %S/Inputs -c -o %t/rdar80704984.o
// RUN: %target-build-swift -Xfrontend -disable-availability-checking -import-objc-header %S/Inputs/rdar80704984.h -Xlinker %t/rdar80704984.o -parse-as-library %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

func run1(on object: PFXObject) async throws {
  do {
    try await object.enqueueErroryRequest()
    fatalError();
  } catch let error {
    // CHECK: Domain=d Code=1
    print(error)
  }
}
func run2(on object: PFXObject) async throws {
  // CHECK: (0, 1)
  print(try await object.enqueueSyncSuccessfulErroryRequest())
}

func run3(on object: PFXObject) async throws {
  // CHECK: (0, 2)
  print(try await object.enqueueAsyncSuccessfulErroryRequest())
}

func runAll(on object: PFXObject) async throws {
  do {
    try await object.enqueueErroryRequest()
    fatalError();
  } catch let error {
    // CHECK: Domain=d Code=1
    print(error)
  }
  // CHECK: (0, 1)
  print(try await object.enqueueSyncSuccessfulErroryRequest())
  // CHECK: (0, 2)
  print(try await object.enqueueAsyncSuccessfulErroryRequest())
}

@main struct Main {
  static func main() async throws {
    let object = PFXObject()
    try await run1(on: object)
    try await run2(on: object)
    try await run3(on: object)
    try await runAll(on: object)
  }
}
