// RUN: %empty-directory(%t)
// RUN: %target-build-swift  -target %target-swift-5.1-abi-triple %s -parse-as-library -module-name main -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: executable_test
// REQUIRES: OS=macosx

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: OS=windows-msvc

actor MyActor {
  func test(_ closure: nonisolated(nonsending) () async -> Void) async {
    self.assertIsolated()
    await closure()
  }
}

nonisolated func test() async {
  let a = MyActor()
  let x = 1

  await a.test { [x] in
    let iso = #isolation
    // CHECK: Optional(main.MyActor)
    print(iso)
    assert(iso === a)
    iso!.assertIsolated()
    a.assertIsolated()
  }

  await a.test {
    let iso = #isolation
    // CHECK: Optional(main.MyActor)
    print(iso)
    assert(iso === a)
    iso!.assertIsolated()
    a.assertIsolated()
  }
}

@main struct Main {
  static func main() async {
    await test()
  }
}
