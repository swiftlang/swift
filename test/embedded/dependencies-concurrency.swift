// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target %target-cpu-apple-macos14 -disable-availability-checking -parse-as-library -enable-experimental-feature Embedded %s -c -o %t/a.o
// RUN: %target-clang %t/a.o -o %t/a.out %swift_obj_root/lib/swift/embedded/%target-cpu-apple-macos/libswift_Concurrency.a -dead_strip

// RUN: grep DEP\: %s | sed 's#// DEP\: ##' | sort > %t/allowed-dependencies.txt

// RUN: %llvm-nm --undefined-only --format=just-symbols %t/a.out | sort | tee %t/actual-dependencies.txt

// Fail if there is any entry in actual-dependencies.txt that's not in allowed-dependencies.txt
// RUN: test -z "`comm -13 %t/allowed-dependencies.txt %t/actual-dependencies.txt`"

// DEP: __ZNSt3__111this_thread9sleep_forERKNS_6chrono8durationIxNS_5ratioILl1ELl1000000000EEEEE
// DEP: __ZNSt3__112basic_stringIcNS_11char_traitsIcEENS_9allocatorIcEEE6appendEPKc
// DEP: __ZNSt3__112basic_stringIcNS_11char_traitsIcEENS_9allocatorIcEEE6appendEPKcm
// DEP: __ZNSt3__112basic_stringIcNS_11char_traitsIcEENS_9allocatorIcEEE6insertEmPKc
// DEP: __ZNSt3__112basic_stringIcNS_11char_traitsIcEENS_9allocatorIcEEED1Ev
// DEP: __ZNSt3__16chrono12steady_clock3nowEv
// DEP: __ZNSt3__19to_stringEj
// DEP: __ZNSt3__19to_stringEy
// DEP: __ZdlPv
// DEP: __Znwm
// DEP: ___assert_rtn
// DEP: ___stack_chk_fail
// DEP: ___stack_chk_guard
// DEP: ___stderrp
// DEP: __availability_version_check
// DEP: _abort
// DEP: _asl_log
// DEP: _dispatch_once_f
// DEP: _dlsym
// DEP: _exit
// DEP: _fclose
// DEP: _fopen
// DEP: _fprintf
// DEP: _fread
// DEP: _free
// DEP: _fseek
// DEP: _ftell
// DEP: _malloc
// DEP: _memcpy
// DEP: _memset_s
// DEP: _os_unfair_lock_lock
// DEP: _os_unfair_lock_unlock
// DEP: _posix_memalign
// DEP: _pthread_equal
// DEP: _pthread_getspecific
// DEP: _pthread_self
// DEP: _pthread_setspecific
// DEP: _putchar
// DEP: _rewind
// DEP: _sscanf
// DEP: _strlen
// DEP: _vfprintf
// DEP: _vsnprintf
// DEP: _write
// DEP: dyld_stub_binder

// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx

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
