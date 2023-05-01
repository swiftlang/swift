// REQUIRES: executable_test
// REQUIRES: asan_runtime
// UNSUPPORTED: OS=windows-msvc

// Check with recovery instrumentation and the runtime option to continue execution.
// RUN: %target-swiftc_driver %s -g -sanitize=address -sanitize-recover=address -import-objc-header %S/asan_interface.h -emit-ir -o %t.asan_recover.ll
// RUN: %FileCheck -check-prefix=CHECK-IR -input-file=%t.asan_recover.ll %s
// RUN: %target-swiftc_driver %s -g -sanitize=address -sanitize-recover=address -import-objc-header %S/asan_interface.h -o %t_asan_recover
// RUN: %target-codesign %t_asan_recover
// RUN: env %env-ASAN_OPTIONS=halt_on_error=0 %target-run %t_asan_recover > %t_asan_recover.stdout 2> %t_asan_recover.stderr
// RUN: %FileCheck --check-prefixes=CHECK-COMMON-STDERR,CHECK-RECOVER-STDERR -input-file=%t_asan_recover.stderr %s
// RUN: %FileCheck --check-prefixes=CHECK-COMMON-STDOUT,CHECK-RECOVER-STDOUT -input-file=%t_asan_recover.stdout %s

// Check with recovery instrumentation but without runtime option to continue execution.
// RUN: env %env-ASAN_OPTIONS=abort_on_error=0,halt_on_error=1 not %target-run %t_asan_recover > %t_asan_no_runtime_recover.stdout 2> %t_asan_no_runtime_recover.stderr
// RUN: %FileCheck --check-prefixes=CHECK-COMMON-STDERR -input-file=%t_asan_no_runtime_recover.stderr %s
// RUN: %FileCheck --check-prefixes=CHECK-COMMON-STDOUT,CHECK-NO-RECOVER-STDOUT -input-file=%t_asan_no_runtime_recover.stdout %s

// Check that without recovery instrumentation and runtime option to continue execution that error recovery does not happen.
// RUN: %target-swiftc_driver %s -g -sanitize=address -import-objc-header %S/asan_interface.h -o %t_asan_no_recover
// RUN: %target-codesign %t_asan_no_recover
// RUN: env %env-ASAN_OPTIONS=abort_on_error=0,halt_on_error=0 not %target-run %t_asan_no_recover > %t_asan_no_recover.stdout 2> %t_asan_no_recover.stderr
// RUN: %FileCheck --check-prefixes=CHECK-COMMON-STDERR -input-file=%t_asan_no_recover.stderr %s
// RUN: %FileCheck --check-prefixes=CHECK-COMMON-STDOUT,CHECK-NO-RECOVER-STDOUT -input-file=%t_asan_no_recover.stdout %s

// We need to test reads via instrumentation not via runtime so try to check
// for calls to unwanted interceptor/runtime functions.
// CHECK-IR-NOT: call {{.+}} @__asan_memcpy
// CHECK-IR-NOT: call {{.+}} @memcpy

// FIXME: We need this so we can flush stdout but this won't
// work on other Platforms (e.g. Microsoft Windows).
#if canImport(Glibc)
    import Glibc
#else
    import Darwin.C
#endif

// CHECK-COMMON-STDOUT: START
print("START")
fflush(stdout)

let size:Int = 128;

func foo(_ rawptr:UnsafeMutablePointer<UInt8>) {
  print("Read second element:\(rawptr.advanced(by: 1).pointee)")
  fflush(stdout)
}

// In this test we need multiple issues to occur that ASan can detect.
// Allocating a buffer and artificially poisoning it seems like the best way to
// test this because there's no undefined behavior happening. Hopefully this
// means that the program behaviour after ASan catches an issue should be
// consistent. If we did something different like two use-after-free issues the
// behaviour could be very unpredictable resulting in a flakey test.
var x = UnsafeMutablePointer<UInt8>.allocate(capacity: size)
x.initialize(repeating: 0, count: size)
__asan_poison_memory_region(UnsafeMutableRawPointer(x), size)

// Perform accesses that ASan will catch. Note it is important here that
// the reads are performed **in** the instrumented code so that the
// instrumentation catches the access to poisoned memory. I tried doing:
//
// ```
// var x = x.advanced(by: 0).pointee
// print(x)
// ```
//
// However, this generated code that's called into memcpy rather than performing
// a direct read which meant that ASan caught an issue via its interceptors
// rather than from instrumentation, which does not test the right thing here.
//
// Doing:
//
// ```
// print("Read first element:\(x.advanced(by: 0).pointee)")
// ```
//
// seems to do the right thing right now but this seems really fragile.

// First error
// NOTE: Testing for stackframe `#0` should ensure that the poison read
// happened in instrumentation and not in an interceptor.
// CHECK-COMMON-STDERR: AddressSanitizer: use-after-poison
// CHECK-COMMON-STDERR: #0 0x{{.+}} in main{{.*}}
print("Read first element:\(x.advanced(by: 0).pointee)")
fflush(stdout)
// CHECK-RECOVER-STDOUT: Read first element:0

// Second error
// NOTE: Very loose regex is to accommodate if name demangling
// fails. rdar://problem/57235673
// CHECK-RECOVER-STDERR: AddressSanitizer: use-after-poison
// CHECK-RECOVER-STDERR: #0 0x{{.+}} in {{.*}}foo{{.*}}
// CHECK-RECOVER-STDOUT: Read second element:0
foo(x)
__asan_unpoison_memory_region(UnsafeMutableRawPointer(x), size)

x.deallocate();
// CHECK-NO-RECOVER-STDOUT-NOT: DONE
// CHECK-RECOVER-STDOUT: DONE
print("DONE")
fflush(stdout)
