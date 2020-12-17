// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency) | %FileCheck %s --dump-input=always
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: OS=macosx
// REQUIRES: CPU=x86_64

import Dispatch

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#endif

func test_sum_nextOnCompleted() async {
    let numbers = [1, 2, 3, 4, 5]
    let expected = numbers.reduce(0, +)

    let sum = await try! Task.withGroup(resultType: Int.self) { (group) async -> Int in
      for n in numbers {
        await group.add { () async -> Int in
          print("  complete group.add { \(n) }")
          return n
        }
      }

      // We specifically want to await on completed child tasks in this test,
      // so give them some time to complete before we hit group.next()
      sleep(2)

      var sum = 0
      do {
        while let r = await try group.next() {
          fputs("error: \(#function)[\(#file):\(#line)]: next: \(r)\n", stderr)
          print("next: \(r)")
//          DispatchQueue.main.sync { // TODO: remove once executors/actors are a thing
            sum += r
//          }
          print("sum: \(sum)")
        }
      } catch {
        print("ERROR: \(error)")
      }

//      assert(group.isEmpty, "Group must be empty after we consumed all tasks")

      fputs("error: \(#function)[\(#file):\(#line)]: group returning: \(sum)", stderr)
      print("task group returning: \(sum)")
      return sum
    }

    // The completions may arrive in any order, we make no strong guarantees about it:
    // CON: CHECK-DAG: complete group.add { [[N1:[0-9]+]] }
    // CON: CHECK-DAG: complete group.add { [[N2:[0-9]+]] }
    // CON: CHECK-DAG: complete group.add { [[N3:[0-9]+]] }
    // CON: CHECK-DAG: complete group.add { [[N4:[0-9]+]] }
    // CON: CHECK-DAG: complete group.add { [[N5:[0-9]+]] }
    // CON: CHECK-DAG: next: [[N1]]
    // CON: CHECK-DAG: next: [[N2]]
    // CON: CHECK-DAG: next: [[N3]]
    // CON: CHECK-DAG: next: [[N4]]
    // CON: CHECK-DAG: next: [[N5]]

    // CHECK: sum: 15
    //
    // CHECK: task group returning: 15

    // CHECK: result: 15
    print("result: \(sum)")
}

runAsyncAndBlock(test_sum_nextOnCompleted)
