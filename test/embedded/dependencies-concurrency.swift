// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -target %target-cpu-apple-macos14 -disable-availability-checking -parse-as-library -enable-experimental-feature Embedded %t/test.swift -c -o %t/a.o
// RUN: %target-clang %t/a.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%target-cpu-apple-macos -lswift_Concurrency -lswift_ConcurrencyDefaultExecutor -dead_strip

// RUN: %llvm-nm --undefined-only --format=just-symbols %t/a.out | sort | tee %t/actual-dependencies.txt

// Fail if there is any entry in actual-dependencies.txt that's not in allowed-dependencies.txt
// RUN: comm -13 %t/allowed-dependencies.txt %t/actual-dependencies.txt > %t/extra.txt
// RUN: test ! -s %t/extra.txt

//--- allowed-dependencies.txt
__ZNSt3__16chrono12steady_clock3nowEv
___assert_rtn
___error
___stack_chk_fail
___stack_chk_guard
_abort
_exit
_free
_malloc
_memcpy
_memmove
_memset
_memset_s
_nanosleep
_posix_memalign
_putchar
_puts
_strlen
_strncpy
_vprintf
_vsnprintf

//--- test.swift
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

import _Concurrency

public func test() async -> Int {
  print("test")
  let t = Task {
    print("return 42")
    return 42
  }
  print("await")
  let v = await t.value
  print("return")
  return v
}

@main
struct Main {
  static func main() async {
    print("main")
    // CHECK: main
    let t = Task {
      print("task")
      let x = await test()
      print(x == 42 ? "42" : "???")
    }
    print("after task")
    await t.value
    // CHECK-NEXT: after task
    // CHECK-NEXT: task
    // CHECK-NEXT: test
    // CHECK-NEXT: await
    // CHECK-NEXT: return 42
    // CHECK-NEXT: return
    // CHECK-NEXT: 42
  }
}
