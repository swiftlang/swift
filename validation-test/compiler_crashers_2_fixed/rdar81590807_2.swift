// RUN: %empty-directory(%t)
// RUN: %target-clang %S/Inputs/rdar81590807_2.m -I %S/Inputs -c -o %t/rdar81590807_2.o
// RUN: %target-build-swift -Xfrontend -disable-availability-checking -import-objc-header %S/Inputs/rdar81590807_2.h -Xlinker %t/rdar81590807_2.o -parse-as-library %s -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: OS=macosx || OS=ios

// rdar://82123254
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

func run(on object: PFXObject) async throws {
  // CHECK: syncSuccess
  print("\(try await object.findAnswerSyncSuccess())\n")
  // CHECK: asyncSuccess
  print("\(try await object.findAnswerAsyncSuccess())\n")
  do {
    try await object.findAnswerSyncFail()
    // CHECK-NOT: oh no syncFail
    print("\("oh no syncFail")\n")
  }
  catch let error {
    // CHECK: Error Domain=syncFail Code=1 "(null)"
    print("\(error)\n")
  }
  do {
    try await object.findAnswerAsyncFail()
    // CHECK-NOT: oh no asyncFail
    print("\("oh no asyncFail")\n")
  }
  catch let error {
    // CHECK: Error Domain=asyncFail Code=2 "(null)"
    print("\(error)\n")
  }
  // CHECK: <<>>
  print("<<\(try await object.findAnswerIncorrect())>>\n")
}

@main struct Main {
  static func main() async throws {
    let object = PFXObject()
    try await run(on: object)
  }
}
