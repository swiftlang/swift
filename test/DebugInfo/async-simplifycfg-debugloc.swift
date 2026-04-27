// RUN: %target-swift-frontend %s -emit-ir -g -Onone -o - \
// RUN:    -module-name M -disable-availability-checking \
// RUN:    -parse-as-library | %FileCheck %s

// REQUIRES: concurrency

// Verify that the early return inside an async function retains a real
// instruction at its debug location.

func fib(_ n: Int) async -> Int {
  if n < 2 {
    // CHECK-DAG: ![[RET_LOC:[0-9]+]] = !DILocation(line: [[@LINE+2]],
    // CHECK-DAG: br label {{.*}}, !dbg ![[RET_LOC]]
    return 1
  }
  let task1 = Task {
    return await fib(n - 1)
  }
  let task2 = Task {
    return await fib(n - 2)
  }
  return await task1.value + task2.value
}
