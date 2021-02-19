// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

struct Boom: Error {}
struct IgnoredBoom: Error {}

func one() async -> Int { 1 }

func boom() async throws -> Int {
  throw Boom()
}

func pprint(_ m: String, file: String = #file, line: UInt = #line) {
//  fputs("[\(file):\(line)] \(m)\n", stderr)
  print(m)
}


func test_taskGroup_throws() async {
  do {
    pprint("start \(#function)")
    let got = try! await Task.withGroup(resultType: Int.self) {
      group async throws -> Int in
      await group.add { await one() }
      await group.add { try await boom()  }

      do {
        while let r = try await group.next() {
          pprint("next: \(r)")
        }
      } catch {
        pprint("error caught in group: \(error)")

        await group.add { () async -> Int in
          let c = await Task.__unsafeCurrentAsync().isCancelled
          print("task 3 (cancelled: \(c))")
          return 3
        }

        guard let got = try! await group.next() else {
          pprint("task group failed to get 3 (:\(#line))")
          return 0
        }

        pprint("task group next: \(got)")

        if got == 1 {
          // the previous 1 completed before the 3 we just submitted,
          // we still want to see that three so let's await for it
          guard let third = try! await group.next() else {
            pprint("task group failed to get 3 (:\(#line))")
            return got
          }

          pprint("task group returning normally: \(third)")
          return third
        } else {
          pprint("task group returning normally: \(got)")
          return got
        }
      }

      fatalError("Should have thrown and handled inside the catch block")
    }

    // Optionally, we may get the first result back before the failure:
    // COM: next: 1
    // CHECK: error caught in group: Boom()
    // CHECK: task 3 (cancelled: false)
    // CHECK: task group returning normally: 3
    // CHECK: got: 3

    pprint("got: \(got)")
  } catch {
    pprint("rethrown: \(error)")
    fatalError("Expected recovered result, but got error: \(error)")
  }
}


@main struct Main {
  static func main() async {
    await test_taskGroup_throws()
  }
}
