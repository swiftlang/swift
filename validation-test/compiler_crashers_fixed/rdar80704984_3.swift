// RUN: %empty-directory(%t)
// RUN: %target-clang %S/Inputs/rdar80704984_3.m -I %S/Inputs -c -o %t/rdar80704984_3.o
// RUN: %target-build-swift -Xfrontend -disable-availability-checking -import-objc-header %S/Inputs/rdar80704984_3.h -Xlinker %t/rdar80704984_3.o -parse-as-library %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

// rdar://82123254
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

func run1(on object: PFXObject) async throws {
  do {
    _ = try await object.enqueueFailingRequest(with: nil)
  }
  catch let error {
    // CHECK: Domain=d Code=1 
    print(error)
  }
}

func run2(on object: PFXObject) async throws {
    // CHECK: (0, 2)
    print(try await object.enqueuePassingRequest(with: nil))
  
}

func run3(on object: PFXObject) async throws {
  do {
    _ = try await object.enqueueFailingRequest(with: nil, completionTimeout: 57.0)
  }
  catch let error {
    // CHECK: Domain=d Code=2 
    print(error)
  }
}

func run4(on object: PFXObject) async throws {
    // CHECK: (0, 3)
    print(try await object.enqueuePassingRequest(with: nil, completionTimeout: 57.0))
  
}

@main struct Main {
  static func main() async throws {
    let object = PFXObject()
    try await run1(on: object)
    try await run2(on: object)
    try await run3(on: object)
    try await run4(on: object)
  }
}
