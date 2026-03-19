// RUN: %empty-directory(%t)
// RUN: %target-clang -fobjc-arc %S/Inputs/objc_async.m -c -o %t/objc_async_objc.o
// RUN: %target-build-swift -target %target-swift-5.1-abi-triple -parse-as-library -module-name main -import-objc-header %S/Inputs/objc_async.h %s %t/objc_async_objc.o -o %t/objc_async
// RUN: %target-codesign %t/objc_async
// RUN: %target-run %t/objc_async | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: objc_interop

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

func computeTest() async {
  let obj = MyNSInterfaceWithCallbackFunc()
  let result = await obj.compute(1738)
  print("finishing \(result)")
}

func farmTest() async {
  let farm = Farm()
  let dogNumber = await farm.doggo
  print("dog number = \(dogNumber)")
  do {
    let _ = try await farm.catto
  } catch {
    print("caught exception")
  }
}

class AsyncOverrideSubclass: MyNSInterfaceWithCallbackFunc {
  override func compute(_ x: Int) async -> Int {
    print("called into override")
    return 219
  }
}

class CompletionHandlerOverrideSubclass: MutableMyNSInterfaceWithCallbackFunc_v2 {
  override func compute(_ x: Int, completionHandler: @escaping (Int) -> Void) {
    print("called again into override")
    completionHandler(20721)
  }
}

@main struct Main {
  static func main() async {
    // CHECK: starting 1738
    // CHECK-NEXT: finishing 679
    await computeTest()

    // CHECK-NEXT: getting dog
    // CHECK-NEXT: dog number = 123
    // CHECK-NEXT: obtaining cat has failed!
    // CHECK-NEXT: caught exception
    await farmTest()

    // CHECK-NEXT: called into override
    let asyncResult = callComputeAndWaitSemaphore(AsyncOverrideSubclass(), 1738)
    // CHECK-NEXT: async override result: 219
    print("async override result: \(asyncResult)")

    // CHECK-NEXT: called again into override
    let handlerResult = callComputeAndWaitSemaphore(CompletionHandlerOverrideSubclass(), 1738)
    // CHECK-NEXT: handler override result: 20721
    print("handler override result: \(handlerResult)")
  }
}
