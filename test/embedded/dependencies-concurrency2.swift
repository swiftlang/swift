// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -target %target-cpu-apple-macos14 -disable-availability-checking -parse-as-library -enable-experimental-feature Embedded %t/test.swift -c -o %t/a.o
// RUN: %target-clang -nostdlib -lSystem %t/a.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%target-cpu-apple-macos -lswift_Concurrency -dead_strip -Wl,-undefined,dynamic_lookup

// RUN: %llvm-nm --undefined-only --format=just-symbols %t/a.out | sort | tee %t/actual-dependencies.txt

// Fail if there is any entry in actual-dependencies.txt that's not in allowed-dependencies.txt
// RUN: comm -13 %t/allowed-dependencies.txt %t/actual-dependencies.txt > %t/extra.txt
// RUN: test ! -s %t/extra.txt

//--- allowed-dependencies.txt
___assert_rtn
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
_posix_memalign
_putchar
_puts
_strlen
_strncpy
_swift_task_asyncMainDrainQueueImpl
_swift_task_enqueueGlobalImpl
_swift_task_getMainExecutorImpl
_swift_task_asyncMainDrainQueueImpl
_swift_task_checkIsolatedImpl
_swift_task_donateThreadToGlobalExecutorUntilImpl
_swift_task_enqueueGlobalImpl
_swift_task_enqueueGlobalWithDeadlineImpl
_swift_task_enqueueGlobalWithDelayImpl
_swift_task_enqueueMainExecutorImpl
_swift_task_getMainExecutorImpl
_swift_task_isMainExecutorImpl
_vprintf
_vsnprintf
dyld_stub_binder

//--- test.swift
// REQUIRES: swift_in_compiler
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
    let t = Task {
      print("task")
      let x = await test()
      print(x == 42 ? "42" : "???")
    }
    print("after task")
    await t.value
  }
}
