// RUN: %empty-directory(%t)
// RUN: %target-clang %S/Inputs/rdar81590807.m -I %S/Inputs -c -o %t/rdar81590807.o
// RUN: %target-build-swift -Xfrontend -disable-availability-checking -import-objc-header %S/Inputs/rdar81590807.h -Xlinker %t/rdar81590807.o -parse-as-library %s -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main > %t/log 2>&1 || true
// RUN: %FileCheck %s < %t/log

// Unsupported because the crash on continueIncorrect is just an illegal 
// instruction rather than a nice fatal error.
// UNSUPPORTED: swift_test_mode_optimize
// UNSUPPORTED: swift_test_mode_optimize_size

// REQUIRES: executable_test
// REQUIRES: objc_interop

// rdar://82123254
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

func run(on object: PFXObject) async throws {
  // CHECK: passSync
  let cl1 = try await object.continuePassSync()
  cl1()
  // CHECK: passAsync
  let cl2 = try await object.continuePassAsync()
  cl2()
  do {
    let cl = try await object.continueFailSync()
    // CHECK-NOT: oh no failSync
    fputs("oh no failSync\n", stderr)
  }
  catch let error {
    // CHECK: Error Domain=failSync Code=1 "(null)"
    fputs("\(error)\n", stderr)
  }
  do {
    let cl = try await object.continueFailAsync()
    // CHECK-NOT: oh no failAsync
    fputs("oh no failAsync\n", stderr)
  }
  catch let error {
    // CHECK: Error Domain=failAsync Code=2 "(null)"
    fputs("\(error)\n", stderr)
  }
  // CHECK: Fatal error: Unexpectedly found nil while implicitly unwrapping an Optional value
  print(try await object.continueIncorrect())
}

@main struct Main {
  static func main() async throws {
    let object = PFXObject()
    try await run(on: object)
  }
}
