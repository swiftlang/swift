// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency %import-libdispatch -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency

// UNSUPPORTED: OS=windows-msvc
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: use_os_stdlib

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func printWaitPrint(_ int: Int) async -> Int {
  print("start, cancelled:\(Task.isCancelled), id:\(int)")
  while !Task.isCancelled {
    await Task.sleep(100_000)
  }
  print("done, cancelled:\(Task.isCancelled), id:\(int)")
  return int
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func test() async {
  let h = detach {
    await printWaitPrint(0)
  }

  let handle = detach {
    print("detached run, cancelled:\(Task.isCancelled)")

    // these tasks will keep spinning until they are cancelled
    async let one = printWaitPrint(1)
    print("spawned: 1")
    async let two = printWaitPrint(2)
    print("spawned: 2")

    h.cancel()

    let first = await one
    print("awaited: 1: \(first)")
    let second = await two
    print("awaited: 2: \(second)")

    // is immediately cancelled, since if we got here one and two completed,
    // which means we're cancelled and thus children should be as well.
    async let three = printWaitPrint(3)
    let third = await three

    print("exit detach")
  }

  await h.get()

  print("cancel")
  handle.cancel()

  // CHECK: detached run, cancelled:false
  // the 1 and 2 tasks are racing so we don't check the specific IDs for them
  // CHECK: start, cancelled:false
  // CHECK: start, cancelled:false
  // CHECK: cancel
  // CHECK: done, cancelled:true
  // CHECK: done, cancelled:true
  // CHECK: start, cancelled:true, id:3
  // CHECK: done, cancelled:true, id:3
  // CHECK: exit detach
  // CHECK: exit

  await handle.get()
  print("exit")
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@main struct Main {
  static func main() async {
    await test()
  }
}
