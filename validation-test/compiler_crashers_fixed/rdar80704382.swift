// RUN: %empty-directory(%t)
// RUN: %target-clang %S/Inputs/rdar80704382.m -I %S/Inputs -c -o %t/rdar80704382.o
// RUN: %target-build-swift -Xfrontend -disable-availability-checking -import-objc-header %S/Inputs/rdar80704382.h -Xlinker %t/rdar80704382.o -parse-as-library %s -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

// rdar://82123254
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

func run() async throws {
  // CHECK: item_id
  // CHECK: file_id
  print(try await PFXObject.identifierForUserVisibleFile(at: URL(fileURLWithPath: "/tmp/file")))
}

@main struct Main {
  static func main() async throws {
    try await run()
  }
}
