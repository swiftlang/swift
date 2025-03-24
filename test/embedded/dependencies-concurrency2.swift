// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target %target-cpu-apple-macos14 -disable-availability-checking -parse-as-library -enable-experimental-feature Embedded %s -c -o %t/a.o
// RUN: %target-clang -nostdlib -lSystem %t/a.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%target-cpu-apple-macos -lswift_Concurrency -dead_strip -Wl,-undefined,dynamic_lookup

// RUN: grep DEP\: %s | sed 's#// DEP\: ##' | sort > %t/allowed-dependencies.txt

// RUN: %llvm-nm --undefined-only --format=just-symbols %t/a.out | sort | tee %t/actual-dependencies.txt

// Fail if there is any entry in actual-dependencies.txt that's not in allowed-dependencies.txt
// RUN: test -z "`comm -13 %t/allowed-dependencies.txt %t/actual-dependencies.txt`"

// DEP: ___assert_rtn
// DEP: ___stack_chk_fail
// DEP: ___stack_chk_guard
// DEP: _abort
// DEP: _exit
// DEP: _free
// DEP: _malloc
// DEP: _memcpy
// DEP: _memmove
// DEP: _memset
// DEP: _memset_s
// DEP: _posix_memalign
// DEP: _putchar
// DEP: _puts
// DEP: _strlen
// DEP: _strncpy
// DEP: _swift_task_asyncMainDrainQueueImpl
// DEP: _swift_task_enqueueGlobalImpl
// DEP: _swift_task_getMainExecutorImpl
// DEP: _swift_task_asyncMainDrainQueueImpl
// DEP: _swift_task_checkIsolatedImpl
// DEP: _swift_task_donateThreadToGlobalExecutorUntilImpl
// DEP: _swift_task_enqueueGlobalImpl
// DEP: _swift_task_enqueueGlobalWithDeadlineImpl
// DEP: _swift_task_enqueueGlobalWithDelayImpl
// DEP: _swift_task_enqueueMainExecutorImpl
// DEP: _swift_task_getMainExecutorImpl
// DEP: _swift_task_isMainExecutorImpl
// DEP: _vprintf
// DEP: _vsnprintf
// DEP: dyld_stub_binder

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
