// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target %target-cpu-apple-macos14 -disable-availability-checking -parse-as-library -enable-experimental-feature Embedded %s -c -o %t/a.o -Osize
// RUN: %target-clang -x c -std=c11 -I %swift_obj_root/include -c %S/Inputs/executor.c -o %t/executor.o -Oz -DNDEBUG -DDEBUG_EXECUTOR=0
// RUN: %target-clang %t/a.o %t/executor.o -o %t/a.out %swift_obj_root/lib/swift/embedded/%target-cpu-apple-macos/libswift_Concurrency.a -dead_strip

// RUN: grep DEP\: %s | sed 's#// DEP\: ##' | sort > %t/allowed-dependencies.txt

// RUN: %llvm-nm --undefined-only --format=just-symbols %t/a.out | sort | tee %t/actual-dependencies.txt

// Fail if there is any entry in actual-dependencies.txt that's not in allowed-dependencies.txt
// RUN: test -z "`comm -13 %t/allowed-dependencies.txt %t/actual-dependencies.txt`"

// DEP: ___stack_chk_fail
// DEP: ___stack_chk_guard
// DEP: _abort
// DEP: _clock_gettime
// DEP: _exit
// DEP: _free
// DEP: _malloc
// DEP: _memcpy
// DEP: _memmove
// DEP: _memset
// DEP: _memset_s
// DEP: _nanosleep
// DEP: _posix_memalign
// DEP: _putchar
// DEP: _puts
// DEP: _strlen
// DEP: _strncpy
// DEP: _vprintf
// DEP: _vsnprintf

// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_stdlib_no_asserts
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
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
    // CHECK: after task
    // CHECK: task
    // CHECK: test
    // CHECK: await
    // CHECK: return 42
    // CHECK: return
    // CHECK: 42
  }
}
